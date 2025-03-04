{ pkgs, lean4, lean4-mode,
# base
withTools ? false, withX ? false,
# build
withShake ? false, withJust ? false,
# lsp
withLsp ? false,
# eye friendly, low-constrat color theme
withSolarized ? false,
# emacs with vim binding
withEmacsEvil ? false,
# vscode with Vim binding
withCodeVim ? false,
# rcs
withGit ? false, withDarcs ? false, withGerrit ? false,
# notes
withOrg ? false,
# mails
withNotMuch ? false,
# language
withC ? false, withPython ? false, withHaskell ? false, withErlang ? false
, withElixir ? false, withGleam ? false, withNix ? false, withAts ? false
, withGLSL ? false, withWGSL ? false, withGo ? false, withTypescript ? false
, withRust ? false, withGraphviz ? false, withLean ? false,
# lisp
withHy ? false, withRacket ? false,
# conf
withDhall ? false, withJson ? false, withYaml ? false, withKubernetes ? false,
# idl
withPlantuml ? false, withProtobuf ? false, withThrift ? false
, withCapnp ? false, withMermaid ? false,
# network
withRest ? false, withRSS ? false,
# packaging
withRpm ? false,
# admin
withAnsible ? false,
# web
withW3M ? false, withGraphQL ? false, withCss ? false,
# text
withMarkdown ? false, withRestructuredText ? false, withSpellChecker ? false,
# wip language
withIdris ? false, withOcaml ? false,
# javascript language
withJavascript ? false, withRescript ? false, withPurescript ? false
, withElm ? false }:

let
  # utility functions
  when = p: l: if p then l else [ ];
  elisp = name: (builtins.readFile (./elisp + "/${name}.el"));

  # A module definition:
  module = s:
    ({
      enabled = false;
      name = "module-name";
      url = "info-url";
      doc = "usage";
      # list of derivations
      buildInputs = [ ];
      # emacs lisp configuration and package callback
      emacsConfig = "";
      emacsPkgs = epkgs: [ ];
      # vim plugins
      vimConfig = "";
      vimPkgs = vpkgs: [ ];
      # vscode extensions
      vscodeExtensions = vsext: [ ];
    } // s);

  # devenv modules
  modules = builtins.filter (m: m.enabled) (map module [
    {
      enabled = withX && withSolarized;
      name = "solarized";
      url = "https://en.wikipedia.org/wiki/Solarized_(color_scheme)";
      emacsPkgs = epkgs: [ epkgs.solarized-theme ];
      buildInputs = [ pkgs.xorg.xprop ];
    }
    {
      enabled = withGit || withGerrit;
      name = "git";
      minimal = true;
      emacsPkgs = epkgs: [
        epkgs.magit
        # epkgs.forge
        epkgs.ghub
        epkgs.git-gutter
      ];
      buildInputs = [ pkgs.openssh pkgs.git pkgs.gitAndTools.hub ];
    }
    {
      enabled = withGerrit;
      buildInputs = [ pkgs.git-review ];
    }
    {
      enabled = withSpellChecker;
      buildInputs = [ pkgs.aspell pkgs.aspellDicts.en ];
      emacsPkgs = epkgs: [ epkgs.flymake-aspell ];
    }
    {
      enabled = withTools;
      buildInputs = with pkgs; [
        which
        procps
        iproute
        rsync
        ripgrep
        man
        findutils
        libnotify
        htop
        tini
      ];
    }
    {
      enabled = withShake;
      buildInputs = with pkgs; [ haskellPackages.shake ];
    }
    {
      enabled = withJust;
      buildInputs = [ pkgs.just ];
      emacsPkgs = epkgs: [ epkgs.just-mode epkgs.justl ];
    }
    {
      enabled = withGraphviz;
      emacsPkgs = epkgs: [ epkgs.graphviz-dot-mode ];
    }
    {
      enabled = withDarcs;
      buildInputs = [ pkgs.darcs ];
    }
    {
      enabled = withLean;
      emacsPkgs = epkgs: [ epkgs.lean4-mode ];
      # buildInputs = [ lean4.lean ];
    }
    {
      enabled = withW3M;
      buildInputs = [ pkgs.w3m ];
      emacsPkgs = epkgs: [ epkgs.w3m ];
    }
    {
      enabled = withC;
      buildInputs =
        (with pkgs; [ cmake gcc automake autoconf openssl zlib pkg-config ]);
    }
    {
      enabled = withHy;
      emacsPkgs = epkgs: [ epkgs.hy-mode ];
    }
    {
      enabled = withKubernetes;
      emacsPkgs = epkgs: [ epkgs.kubed ];
    }
    {
      enabled = withGo;
      # buildInputs = [ pkgs.go ];
      emacsPkgs = epkgs: [ epkgs.go-mode ];
    }
    {
      enabled = withRacket;
      emacsPkgs = epkgs: [ epkgs.racket-mode ];
    }
    {
      enabled = withPython;
      minimal = true;
      buildInputs = [
        (pkgs.python311.withPackages (ps:
          with ps;
          let
            ansible = (when withAnsible [ ps.ansible ]);
            notmuch = (when withNotMuch [ ps.notmuch ]);
            sphinx = (when withRestructuredText) [ ps.sphinx ];
          in ansible ++ notmuch ++ sphinx
          ++ [ virtualenv pip mypy black flake8 pyyaml ]))
      ];
      vscodeExtensions = vsext: [ vsext.ms-python.python ];
    }
    {
      enabled = withNix;
      buildInputs = [ pkgs.nixfmt pkgs.nixUnstable ];
      emacsPkgs = epkgs: [ epkgs.nix-mode ];
      vscodeExtensions = vsext:
        (pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "nix";
            publisher = "bbenoist";
            version = "1.0.1";
            sha256 = "0zd0n9f5z1f0ckzfjr38xw2zzmcxg1gjrava7yahg5cvdcw6l35b";
          }
          {
            name = "nixfmt-vscode";
            publisher = "brettm12345";
            version = "0.0.1";
            sha256 = "07w35c69vk1l6vipnq3qfack36qcszqxn8j3v332bl0w6m02aa7k";
          }
        ]);
    }
    {
      enabled = withIdris;
      # buildInputs = [ pkgs.idris2 ];
      emacsPkgs = epkgs: [ epkgs.idris-mode ];
    }
    {
      enabled = withErlang || withElixir;
      # buildInputs = [ pkgs.erlang pkgs.rebar3 ];
      emacsPkgs = epkgs: [ epkgs.erlang ];
    }
    {
      enabled = withElixir;
      # buildInputs = [ pkgs.elixir ];
      emacsPkgs = epkgs: [ epkgs.elixir-mode ];
    }
    {
      enabled = withGleam;
      emacsPkgs = epkgs: [ epkgs.gleam-mode ];
    }
    {
      enabled = withHaskell;
      name = "haskell";
      emacsPkgs = epkgs: [ epkgs.haskell-mode ];
      vscodeExtensions = vsext:
        (pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "haskell";
            publisher = "haskell";
            version = "1.2.0";
            sha256 = "0vxsn4s27n1aqp5pp4cipv804c9cwd7d9677chxl0v18j8bf7zly";
          }
          {
            name = "language-haskell";
            publisher = "justusadam";
            version = "3.4.0";
            sha256 = "0ab7m5jzxakjxaiwmg0jcck53vnn183589bbxh3iiylkpicrv67y";
          }
        ]);
    }
    {
      enabled = withRust;
      name = "rust";
      emacsPkgs = epkgs: [ epkgs.rust-mode ];
      # buildInputs = [ pkgs.cargo pkgs.rustc ];
    }
    {
      enabled = withPurescript;
      buildInputs = [
        pkgs.spago-unstable
        pkgs.purs
        pkgs.nodePackages_latest.purty
        pkgs.esbuild
      ];
      emacsPkgs = epkgs: [ epkgs.purescript-mode epkgs.psci epkgs.psc-ide ];
    }
    {
      enabled = withElm;
      # buildInputs = with pkgs.elmPackages; [ elm elm-format elm-review ];
      emacsPkgs = epkgs: [ epkgs.elm-mode ];
    }
    {
      name = "nodejs";
      enabled = withJavascript || withRescript || withPurescript
        || withTypescript;
      buildInputs = [ pkgs.nodejs ];
    }
    {
      enabled = withRescript;
      emacsPkgs = epkgs:
        [ epkgs.rescript-mode ] ++ (when withLsp [ epkgs.lsp-rescript ]);
    }
    {
      enabled = withTypescript;
      emacsPkgs = epkgs: [ epkgs.typescript-mode ];
    }
    {
      enabled = withEmacsEvil;
      emacsPkgs = epkgs: [ epkgs.evil ];
    }
    {
      name = "ocaml";
      enabled = withOcaml;
      # buildInputs =
      #   [ pkgs.dune_2 pkgs.opam pkgs.ocaml pkgs.ocamlPackages.merlin ];
      emacsPkgs = epkgs: [ epkgs.tuareg ];
    }
    {
      enabled = withX;
      buildInputs = with pkgs; [ fontconfig ];
      emacsPkgs = epkgs: [
        epkgs.all-the-icons
        epkgs.all-the-icons-dired
        # todo: figure out the right font...
        # epkgs.all-the-icons-ivy
        epkgs.all-the-icons-ivy-rich
      ];
    }
    {
      enabled = withJson;
      buildInputs = [ pkgs.jq ];
      emacsPkgs = epkgs: [ epkgs.json-mode epkgs.counsel-jq ];
    }
    {
      enabled = withYaml;
      emacsPkgs = epkgs: [ epkgs.yaml-mode ];
    }
    {
      enabled = withGraphQL;
      emacsPkgs = epkgs: [ epkgs.graphql-mode ];
    }
    {
      enabled = withJson || withYaml || withGraphQL || withCss || withMarkdown;
      name = "prettier";
      buildInputs = [ pkgs.nodePackages.prettier ];
    }
    {
      enabled = withRpm;
      emacsPkgs = epkgs: [ epkgs.rpm-spec-mode ];
    }
    {
      enabled = withDhall;
      name = "dhall";
      emacsPkgs = epkgs: [ epkgs.dhall-mode ];
      vimConfig = "let g:dhall_format=1";
      vimPkgs = vpkgs: [ vpkgs.dhall-vim ];
      vscodeExtensions = vsext:
        (pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "dhall-lang";
            publisher = "dhall";
            version = "0.0.4";
            sha256 = "0sa04srhqmngmw71slnrapi2xay0arj42j4gkan8i11n7bfi1xpf";
          }
          {
            name = "vscode-dhall-lsp-server";
            publisher = "dhall";
            version = "0.0.4";
            sha256 = "1zin7s827bpf9yvzpxpr5n6mv0b5rhh3civsqzmj52mdq365d2js";
          }
        ]);
      buildInputs = (with pkgs;
        [ dhall dhall-json dhall-docs ] ++ (when withLsp [ dhall-lsp-server ]));
    }
    {
      enabled = withProtobuf;
      emacsPkgs = epkgs: [ epkgs.protobuf-mode ];
    }
    {
      enabled = withCapnp;
      emacsPkgs = epkgs: [ epkgs.capnp-mode ];
    }
    {
      enabled = withRest;
      emacsPkgs = epkgs:
        [ epkgs.restclient ] ++ (when withOrg [ epkgs.ob-restclient ]);
    }
    {
      enabled = withPlantuml;
      # buildInputs = [ pkgs.plantuml pkgs.openjdk pkgs.graphviz ];
      emacsPkgs = epkgs: [ epkgs.plantuml-mode ];
    }
    {
      enabled = withMermaid;
      buildInputs = [ pkgs.mermaid-cli ];
      emacsPkgs = epkgs: [ epkgs.mermaid-mode ];
    }
    {
      enabled = withThrift;
      emacsPkgs = epkgs: [ epkgs.thrift ];
    }
    {
      enabled = withMarkdown;
      emacsPkgs = epkgs:
        [ epkgs.markdown-mode ] ++ (when withOrg [ epkgs.ox-gfm ]);
    }
    {
      enabled = withOrg;
      emacsPkgs = epkgs: [
        epkgs.org
        epkgs.denote
        epkgs.consult-denote
        epkgs.org-modern
        epkgs.org-present
        epkgs.org-ql
        epkgs.org-sidebar
      ];
    }
    {
      enabled = withRSS;
      emacsPkgs = epkgs: [ epkgs.elfeed epkgs.elfeed-org ];
    }
    {
      enabled = withAts;
      # buildInputs = [ pkgs.ats2 pkgs.haskellPackages.ats-format ];
    }
    {
      enabled = withGLSL;
      emacsPkgs = epkgs: [ epkgs.glsl-mode ];
    }
    {
      enabled = withWGSL;
      emacsPkgs = epkgs: [ epkgs.wgsl-mode ];
    }
    {
      enabled = withNotMuch;
      buildInputs = with pkgs; [ notmuch msmtp dovecot_pigeonhole isync ];
      emacsPkgs = epkgs: [ epkgs.notmuch epkgs.ol-notmuch ];
    }
  ]);

  concatModuleText = f: builtins.concatStringsSep "\n" (map f modules);
  concatModuleList = f: builtins.concatLists (map f modules);

  # the fonts
  fonts = pkgs.makeFontsConf {
    fontDirectories = [
      pkgs.iosevka
      pkgs.iosevka-comfy.comfy-wide
      pkgs.iosevka-comfy.comfy
      pkgs.noto-fonts-emoji
      pkgs.roboto
      pkgs.fira-code
    ];
  };

  # the emacs derivation
  emacsDrv = if withX then pkgs.emacs30-pgtk else pkgs.emacs30-nox;

  # Override a package when sources are missing:
  emacsOverride = self: super: {
    lean4-mode = self.melpaBuild {
      pname = "lean4-mode";
      version = "1";
      commit = "1";
      src = lean4-mode;
      packageRequires = with self; [ dash f flycheck magit-section lsp-mode s ];
      recipe = pkgs.writeText "recipe" ''
        (lean4-mode
         :repo "leanprover/lean4-mode"
         :fetcher github
         :files ("*.el" "data"))
      '';
    };
  };

  emacs = ((pkgs.emacsPackagesFor emacsDrv).overrideScope'
    emacsOverride).emacsWithPackages (epkgs:
      (let
        # emacs packages:
        vertico = [
          epkgs.vertico
          epkgs.orderless
          epkgs.consult
          epkgs.embark
          epkgs.embark-consult
          epkgs.cape
          epkgs.corfu
          epkgs.corfu-terminal
        ];
        nano-agenda = pkgs.fetchFromGitHub {
          owner = "rougier";
          repo = "nano-agenda";
          rev = "3d1e280fd94ac1abd630d0c100a860b94472836d";
          sha256 = "sha256-jvCO2vw76wz6mWhYjvLCRzOC0UM2i2zlBUPdQPX78rc=";
        };

        prog = [ epkgs.format-all epkgs.tree-sitter epkgs.tree-sitter-indent ];
        base = [
          (pkgs.runCommand "nano-emacs" { } ''
            mkdir -p $out/share/emacs/site-lisp
            cp ${nano-agenda}/*.el $out/share/emacs/site-lisp/
          '')
          epkgs.treesit-grammars.with-all-grammars
          epkgs.tree-sitter-langs
          epkgs.dirvish
          epkgs.company
          epkgs.vterm
          epkgs.anzu
          epkgs.rainbow-delimiters
          epkgs.diredfl
          epkgs.reformatter
          epkgs.tmr
          epkgs.spacious-padding
          epkgs.fontaine
          epkgs.goggles
          epkgs.tempel
          epkgs.undo-tree
          epkgs.wgrep
          epkgs.diff-hl
          epkgs.dash
          epkgs.dash-functional
          epkgs.f
          epkgs.simpleclip
          epkgs.diminish
          epkgs.ripgrep
          epkgs.deadgrep
          epkgs.bufler
          epkgs.deadgrep
          epkgs.ace-window
          epkgs.ace-link
          epkgs.avy
          epkgs.visual-fill-column
          epkgs.multiple-cursors
          epkgs.which-key
          epkgs.dired-quick-sort
          # epkgs.helpful
          epkgs.plz
        ] ++ vertico ++ prog ++ (when withX [ epkgs.svg-lib ]);
      in base ++ (concatModuleList (m: m.emacsPkgs epkgs))));

  # vim
  vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.packages.myVimPackage = {
      # loaded on launch
      start = concatModuleList (m: m.vimPkgs pkgs.vimPlugins);
    };
    vimrcConfig.customRC = ''
      filetype indent plugin on
      colorscheme desert
      set tabstop=4
      set expandtab
      set tabstop=4
      set shiftwidth=4
      set softtabstop=4

      set listchars=trail:·
      set list
    '' + (concatModuleText (m: m.vimConfig));
  };

  # code
  vscode = pkgs.vscode-with-extensions.override {
    vscodeExtensions =
      (when withCodeVim [ pkgs.vscode-extensions.vscodevim.vim ])
      ++ (concatModuleList (m: m.vscodeExtensions pkgs.vscode-extensions));
  };

  # pycharm
  pycharm = pkgs.jetbrains.pycharm-community;

  # export LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib/:/run/opengl-driver/lib/
  profile = "" + (if withAts then ''
    export ATS_LOADPATH=${pkgs.ats2}/share/emacs/site-lisp/ats2
  '' else
    "") + (if withX then ''
      export FONTCONFIG_FILE=${fonts}
    '' else
      "");

  # devenv derivations collection:
  toolchains = concatModuleList (m: m.buildInputs);

in {
  profile = profile;
  pycharm = pycharm;
  emacs = emacs;
  vim = vim;
  vscode = vscode;
  toolchains = toolchains;
}
