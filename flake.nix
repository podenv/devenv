{
  description = "devenv editor";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/acb77d2bf9a902fe0d3f27c47ab39aa93afb4c2a";
    emacs-overlay.url =
      "github:nix-community/emacs-overlay/e61547b1c4cdf6e6ed72862748397e49df9129ca";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
    lean4.url = "github:leanprover/lean4";
    lean4-mode.url = "github:leanprover/lean4-mode";
    lean4-mode.flake = false;
  };

  outputs = { self, nixpkgs, emacs-overlay, rust-overlay, purescript-overlay
    , lean4, lean4-mode }:
    let
      pkgs = import nixpkgs {
        localSystem = "x86_64-linux";
        overlays = [
          emacs-overlay.overlay
          rust-overlay.overlays.default
          purescript-overlay.overlays.default
        ];
        # allowUnfree for nvidia
        config.allowUnfree = true;
      };

      devenNix = import ./default.nix;
      devent = args: p:
        devenNix (args // {
          pkgs = p;
          lean4 = lean4.packages.x86_64-linux;
          lean4-mode = lean4-mode;
        });

      mkToolchains = args: (devent args pkgs).toolchains;

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
          vscode = (devent args pkgsNonFree).vscode;
          devenv = devent args pkgs;
          nox = devent (args // { withX = false; }) pkgs;

        in pkgs.lib.foldr pkgs.lib.recursiveUpdate { } [
          (mkEditor devenv.emacs "emacs" "emacs")
          (mkEditor nox.emacs "emacs-nox" "emacs")
          (mkEditor devenv.vim "vim" "vim")
          (mkEditor vscode "vscode" "code")
          {
            apps."x86_64-linux"."emacsclient${setName}" = {
              type = "app";
              program = let
                wrapper = pkgs.writeScriptBin "emacsclient-wrapper" ''
                  #!/bin/sh
                  exec ${devenv.emacs}/bin/emacsclient $*
                '';
              in "${wrapper}/bin/emacsclient-wrapper";
            };
          }
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
        withX = true;
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
        withLean = true;
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
        withSpellChecker = true;
      };

      complete = essentials // {
        withOrg = true;
        withNotMuch = true;
        withErlang = true;
        withElixir = true;
        withPurescript = true;
        withGLSL = true;
        withWGSL = true;
        withPlantuml = true;
        withMermaid = true;
        withProtobuf = true;
        withCapnp = true;
        withThrift = true;
        withAnsible = true;
        withGraphQL = true;
        withW3M = true;
        withDarcs = true;
        withHy = true;
        withGraphviz = true;
        withRescript = true;
        withJust = true;
        withRSS = true;
      };

      mkShell = xs:
        pkgs.mkShell {
          buildInputs = with pkgs; [ nix pkg-config zlib gcc ripgrep ] ++ xs;
        };

      nixGLSrc = pkgs.fetchFromGitHub {
        owner = "guibou";
        repo = "nixGL";
        rev = "c917918ab9ebeee27b0dd657263d3f57ba6bb8ad";
        sha256 = "sha256-KCkWZXCjH+C4Kn7fUGSrEl5btk+sERHhZueSsvVbPWc=";
      };
      nixGL = import nixGLSrc { pkgs = pkgs; };

      gfxShell = with pkgs;
        mkShell [
          nixGL.auto.nixGLDefault
          nixGL.auto.nixVulkanNvidia
          nixGL.nixVulkanIntel
          vulkan-tools
          vulkan-loader
          libGLU
          libGL
          glew
          xorg.libX11
          xorg.xrandr
          xorg.libXcursor
          xorg.libXxf86vm
          xorg.libXinerama
          xorg.libXrandr
          xorg.libXi
          xorg.libXext
          glslang
          vulkan-validation-layers
          opusfile
          openal
          xorg.libXdmcp
          vulkan-tools-lunarg
          SDL
          SDL2
          wayland
          wayland-protocols
          glm
          directfb
          gmp
        ];

    in pkgs.lib.foldr pkgs.lib.recursiveUpdate { } [
      { devShell."x86_64-linux" = gfxShell; }
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
