{ pkgs, lean4,
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
, withCapnp ? false,
# network
withRest ? false,
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
      emacsConfig = elisp "solarized";
      emacsPkgs = epkgs: [ epkgs.solarized-theme ];
      buildInputs = [ pkgs.xorg.xprop ];
    }
    {
      enabled = withGit || withGerrit;
      name = "git";
      minimal = true;
      emacsConfig = elisp "magit";
      emacsPkgs = epkgs: [
        epkgs.magit
        epkgs.forge
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
      emacsPkgs = epkgs: [ lean4.lean4-mode ];
      buildInputs = [ lean4.lean ];
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
      emacsPkgs = epkgs: [ epkgs.kubernetes ];
    }
    {
      enabled = withGo;
      buildInputs = [ pkgs.go ];
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
        (pkgs.python310.withPackages (ps:
          with ps;
          let
            ansible = (when withAnsible [ ps.ansible ]);
            notmuch = (when withNotMuch [ ps.notmuch ]);
            sphinx = (when withRestructuredText) [ ps.sphinx ];
          in ansible ++ notmuch ++ sphinx
          ++ [ virtualenv pip mypy black flake8 pyyaml ]))
      ];
      emacsConfig = elisp "python";
      vscodeExtensions = vsext: [ vsext.ms-python.python ];
    }
    {
      enabled = withNix;
      buildInputs = [ pkgs.nixfmt pkgs.nixUnstable ];
      emacsPkgs = epkgs: [ epkgs.nix-mode ];
      emacsConfig = elisp "nix";
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
      buildInputs = [ pkgs.idris2 ];
      emacsPkgs = epkgs: [ epkgs.idris-mode ];
      emacsConfig = elisp "idris";
    }
    {
      enabled = withErlang || withElixir;
      buildInputs = [ pkgs.erlang pkgs.rebar3 ];
      emacsPkgs = epkgs: [ epkgs.erlang ];
    }
    {
      enabled = withElixir;
      buildInputs = [ pkgs.elixir ];
      emacsPkgs = epkgs: [ epkgs.elixir-mode ];
      emacsConfig = elisp "elixir";
    }
    {
      enabled = withGleam;
      emacsPkgs = epkgs: [ epkgs.gleam-mode ];
    }
    {
      enabled = withHaskell;
      name = "haskell";
      emacsConfig = elisp "haskell";
      emacsPkgs = epkgs: [ epkgs.haskell-mode epkgs.ormolu ];
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
      buildInputs = [ pkgs.cargo pkgs.rustc ];
    }
    {
      enabled = withPurescript;
      buildInputs = [ pkgs.spago pkgs.purescript ];
      emacsConfig = elisp "purescript";
      emacsPkgs = epkgs: [ epkgs.purescript-mode epkgs.psci epkgs.psc-ide ];
    }
    {
      enabled = withElm;
      buildInputs = with pkgs.elmPackages; [ elm elm-format elm-review ];
      emacsPkgs = epkgs: [ epkgs.elm-mode ];
    }
    {
      name = "nodejs";
      enabled = withJavascript || withRescript || withPurescript
        || withTypescript;
      buildInputs = [ pkgs.nodejs pkgs.nodePackages.pnpm ];
    }
    {
      enabled = withRescript;
      emacsPkgs = epkgs:
        [ epkgs.rescript-mode ] ++ (when withLsp [ epkgs.lsp-rescript ]);
      emacsConfig = elisp "rescript";
    }
    {
      enabled = withTypescript;
      emacsPkgs = epkgs: [ epkgs.typescript-mode ];
    }
    {
      enabled = withEmacsEvil;
      emacsPkgs = epkgs: [ epkgs.evil ];
      emacsConfig = elisp "evil";
    }
    {
      name = "ocaml";
      enabled = withOcaml;
      buildInputs =
        [ pkgs.dune_2 pkgs.opam pkgs.ocaml pkgs.ocamlPackages.merlin ];
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
      emacsConfig = elisp "gfx";
    }
    {
      enabled = withJson;
      buildInputs = [ pkgs.jq ];
      emacsPkgs = epkgs: [ epkgs.json-mode epkgs.counsel-jq ];
      emacsConfig = elisp "json";
    }
    {
      enabled = withYaml;
      emacsConfig = elisp "yaml";
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
      emacsConfig = elisp "dhall";
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
      buildInputs = (with pkgs; [
        dhall
        dhall-json
        dhall-docs
      ] # pkg is currently broken: ++ (when withLsp [ dhall-lsp-server ])
      );
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
      emacsConfig = if withOrg then elisp "ob-restclient" else "";
      emacsPkgs = epkgs:
        [ epkgs.restclient ] ++ (when withOrg [ epkgs.ob-restclient ]);
    }
    {
      enabled = withPlantuml;
      buildInputs = [ pkgs.plantuml pkgs.openjdk pkgs.graphviz ];
      emacsPkgs = epkgs: [ epkgs.plantuml-mode ];
    }
    {
      enabled = withThrift;
      emacsPkgs = epkgs: [ epkgs.thrift ];
    }
    {
      enabled = withMarkdown;
      emacsConfig = elisp "markdown";
      emacsPkgs = epkgs:
        [ epkgs.markdown-mode ] ++ (when withOrg [ epkgs.ox-gfm ]);
    }
    {
      enabled = withOrg;
      emacsConfig = elisp "org";
      emacsPkgs = epkgs: [ epkgs.org epkgs.org-present epkgs.org-ql ];
    }
    {
      enabled = withAts;
      buildInputs = [ pkgs.ats2 pkgs.haskellPackages.ats-format ];
      emacsConfig = elisp "ats";
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
      emacsPkgs = epkgs: [
        (epkgs.notmuch.override {
          elpaBuild = args:
            epkgs.elpaBuild (args // {
              patches = [
                (pkgs.fetchpatch {
                  url =
                    "https://github.com/TristanCacqueray/notmuch/commit/26f21e89649a2e1abd842fb6b212cb8ec69ff392.patch";
                  sha256 =
                    "sha256-dUQ2Ugyu6wsOpKivwKh3cnpxT03725nhPdW2hYeLVVU=";
                })
              ];
            });
        })
        epkgs.ol-notmuch
      ];
    }
  ]);

  concatModuleText = f: builtins.concatStringsSep "\n" (map f modules);
  concatModuleList = f: builtins.concatLists (map f modules);

  # the fonts
  fonts = pkgs.makeFontsConf {
    fontDirectories =
      [ pkgs.iosevka pkgs.noto-fonts-emoji pkgs.roboto pkgs.fira-code ];
  };

  # the emacs derivation
  emacsDrv = if withX then pkgs.emacs-git else pkgs.emacs-git-nox;

  emacsOverride = self: super: {
    spinner = super.spinner.override {
      elpaBuild = args:
        super.elpaBuild (args // {
          version = "1.7.4";
          src = pkgs.fetchurl {
            url = "https://elpa.gnu.org/packages/spinner-1.7.4.tar";
            sha256 = "140kss25ijbwf8hzflbjz67ry76w2cyrh02axk95n6qcxv7jr7pv";
          };
        });
    };
  };

  emacs = ((pkgs.emacsPackagesFor emacsDrv).overrideScope'
    emacsOverride).emacsWithPackages (epkgs:
      (let
        # the emacs config:
        emacsSite = pkgs.writeText "default.el" ((elisp "base")
          + (elisp "base-extra") + (elisp "format-all")
          + (concatModuleText (m: m.emacsConfig)) + ''
            ;; reset gc to reasonable level
            (setq gc-cons-threshold (* 20 1024 1024))'');

        # emacs packages:
        ivy = [
          epkgs.smex
          epkgs.ivy
          epkgs.counsel
          epkgs.swiper
          epkgs.company
          epkgs.projectile
          epkgs.ivy-rich
        ] ++ (when withX [ epkgs.svg-lib ]);
        nano-agenda = pkgs.fetchFromGitHub {
          owner = "rougier";
          repo = "nano-agenda";
          rev = "3d1e280fd94ac1abd630d0c100a860b94472836d";
          sha256 = "sha256-jvCO2vw76wz6mWhYjvLCRzOC0UM2i2zlBUPdQPX78rc=";
        };

        prog = [ epkgs.format-all epkgs.tree-sitter epkgs.tree-sitter-indent ];
        base = [
          (pkgs.runCommand "default.el" { } ''
            mkdir -p $out/share/emacs/site-lisp
            cp ${emacsSite} $out/share/emacs/site-lisp/default.el
          '')
          (pkgs.runCommand "nano-emacs" { } ''
            mkdir -p $out/share/emacs/site-lisp
            cp ${nano-agenda}/*.el $out/share/emacs/site-lisp/
          '')
          epkgs.use-package
          epkgs.vterm
          epkgs.anzu
          epkgs.rainbow-delimiters
          epkgs.undo-tree
          epkgs.diff-hl
          epkgs.dash
          epkgs.dash-functional
          epkgs.f
          epkgs.simpleclip
          epkgs.diminish
          epkgs.ripgrep
          epkgs.bufler
          epkgs.deadgrep
          epkgs.ace-window
          epkgs.ace-link
          epkgs.avy
          epkgs.visual-fill-column
          epkgs.multiple-cursors
          epkgs.which-key
          epkgs.helpful
        ] ++ ivy ++ prog;
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

  # share the pinned nixpkgs
  nixpkgs_src =
    pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./nixpkgs.json));

in {
  profile = profile;
  pycharm = pycharm;
  emacs = emacs;
  vim = vim;
  vscode = vscode;
  toolchains = toolchains;
}
