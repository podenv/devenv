{
  description = "devenv editor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, emacs-overlay, rust-overlay }:
    let
      pkgs = import nixpkgs {
        localSystem = "x86_64-linux";
        overlays = [ emacs-overlay.overlay rust-overlay.overlays.default ];
      };

      devent = import ./default.nix;

      mkToolchains = args: (devent ({ pkgs = pkgs; } // args)).toolchains;

      mkSet = setName: args:
        let
          mkEditor = editor: name: command: {
            packages."x86_64-linux"."${name}${setName}" = editor;
            apps."x86_64-linux"."${name}${setName}" = {
              type = "app";
              program = let
                wrapper = pkgs.writeScriptBin "${command}-wrapper" ''
                  #!/bin/sh
                  export NIX_PATH=nixpkgs=${nixpkgs}
                  ${devenv.profile}
                  exec ${editor}/bin/${command} $*
                '';
              in "${wrapper}/bin/${command}-wrapper";
            };
          };

          pkgsNonFree = import nixpkgs {
            localSystem = "x86_64-linux";
            config.allowUnfree = true;
          };
          vscode = (devent ({ pkgs = pkgsNonFree; } // args)).vscode;
          devenv = devent ({ pkgs = pkgs; } // args);
          nox = devent ({ pkgs = pkgs; } // args // { withX = false; });

        in pkgs.lib.foldr pkgs.lib.recursiveUpdate { } [
          (mkEditor devenv.emacs "emacs" "emacs")
          (mkEditor nox.emacs "emacs-nox" "emacs")
          (mkEditor devenv.vim "vim" "vim")
          (mkEditor vscode "vscode" "code")
        ];

      get_runtime = name: runtime:
        let
          # It seems like flakes does not enable exporting a list of derivation.
          # Thus instead, we generate the list of store path, so that they can be installed with
          # nix profile install $(nix run .#toolchains)
          wrapper = pkgs.writeScriptBin "print-runtime" ''
            #!/bin/sh
            echo ${builtins.toString (mkToolchains runtime)}
          '';
        in {
          packages."x86_64-linux"."toolchains${name}" = wrapper;
          apps."x86_64-linux"."toolchains${name}" = {
            type = "app";
            program = "${wrapper}/bin/print-runtime";
          };
        };
      get_lang_runtime = name: runtime:
        get_runtime name (runtime // { withLsp = true; });

      base = {
        withTools = true;
        withGit = true;
      };

      essentials = base // {
        withLsp = true;
        withGerrit = true;
        withOrg = true;
        withC = true;
        withSolarized = true;
        withPython = true;
        withNix = true;
        withHaskell = true;
        withGo = true;
        withRust = true;
        withDhall = true;
        withJson = true;
        withYaml = true;
        withRest = true;
        withRpm = true;
        withCss = true;
        withJavascript = true;
        withTypescript = true;
        withMarkdown = true;
        withRestructuredText = true;
        withX = true;
      };

      complete = essentials // {
        withNeuron = true;
        withOrg = true;
        withNotMuch = true;
        withErlang = true;
        withElixir = true;
        withPurescript = true;
        withGleam = true;
        withGLSL = true;
        withKubernetes = true;
        withPlantuml = true;
        withProtobuf = true;
        withThrift = true;
        withAnsible = true;
        withGraphQL = true;
        withIdris = true;
        withOcaml = true;
        withElm = true;
        withW3M = true;
        withDarcs = true;
        withHy = true;
        withGraphviz = true;
        withLean = true;
      };

    in pkgs.lib.foldr pkgs.lib.recursiveUpdate { } [
      (get_runtime "-minimal" base)
      (get_runtime "" essentials)
      (get_runtime "-complete" complete)
      (get_lang_runtime "-haskell" { withHaskell = true; })
      (get_lang_runtime "-purescript" { withPurescript = true; })
      (get_lang_runtime "-rust" { withRust = true; })
      (get_lang_runtime "-go" { withGo = true; })
      (mkSet "-minimal" base)
      (mkSet "" essentials)
      (mkSet "-complete" complete)
    ];
}
