/* nix cheat-sheet:
   # nix lang:
      https://nixos.org/manual/nix/stable/#sec-language-operators
   # Query emacs packages:
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.elpaPackages
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.melpaPackages
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.melpaStablePackages
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.orgPackages
*/

{ pkgs ? import ./nixpkgs.nix,
# base
withTools ? false, withX ? true, withIntel ? false,
# editor
withEmacs ? false, withVSCode ? false, withVim ? false, withPyCharm ? false,
# build
withShake ? false,
# lsp
withLsp ? false,
# eye friendly, low-constrat color theme
withSolarized ? true,
# emacs with vim binding
withEmacsEvil ? false,
# vscode with Vim binding
withCodeVim ? false,
# rcs
withGit ? true, withDarcs ? false,
# notes
withNeuron ? false, withOrg ? false,
# mails
withNotMuch ? false,
# language
withC ? true, withPython ? true, withHaskell ? false, withErlang ? false
, withElixir ? false, withNix ? false, withAts ? false, withGLSL ? false
, withGo ? false, withTypescript ? false, withRust ? false,
# lisp
withHy ? false, withRacket ? false,
# conf
withDhall ? true, withJson ? true, withYaml ? true,
# idl
withPlantuml ? false, withProtobuf ? false, withThrift ? false,
# network
withRest ? false,
# packaging
withRpm ? false,
# admin
withAnsible ? false,
# gfx
withOpenGL ? false, withVulkan ? false,
# web
withW3M ? false, withGraphQL ? false, withCss ? false,
# text
withMarkdown ? true, withRestructuredText ? true, withPdf ? false,
# wip language
withIdris ? false, withOcaml ? false, withReasonNative ? false,
# javascript language
withReason ? false, withPurescript ? false,
# minimal override
minimal ? false, }:

let
  # utility functions
  when = p: l: if p then l else [ ];
  elisp = name: (builtins.readFile (./elisp + "/${name}.el"));

  # A module definition:
  module = s:
    ({
      enabled = false;
      name = "module-name";
      minimal = false;
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
  modules =
    builtins.filter (m: m.enabled && (if minimal then m.minimal else true))
    (map module [
      {
        enabled = withX && withSolarized;
        name = "solarized";
        url = "https://en.wikipedia.org/wiki/Solarized_(color_scheme)";
        emacsConfig = elisp "solarized";
        emacsPkgs = epkgs: [ epkgs.solarized-theme ];
        buildInputs = [ pkgs.xorg.xprop ];
      }
      {
        enabled = withGit;
        name = "git";
        minimal = true;
        emacsConfig = elisp "magit";
        emacsPkgs = epkgs: [ epkgs.magit epkgs.forge epkgs.ghub ];
        buildInputs = [ pkgs.openssh pkgs.git pkgs.gitAndTools.hub ];
      }
      {
        enabled = withTools;
        buildInputs = [
          pkgs.which
          pkgs.procps
          pkgs.iproute
          pkgs.coreutils
          pkgs.rsync
        ];
      }
      {
        enabled = withShake;
        buildInputs = [ pkgs.haskellPackages.shake ];
      }
      {
        enabled = withDarcs;
        buildInputs = [ pkgs.darcs ];
      }
      {
        enabled = withW3M;
        buildInputs = [ pkgs.w3m ];
        emacsPkgs = epkgs: [ epkgs.emacsw3m ];
      }
      {
        enabled = withC;
        buildInputs =
          (with pkgs; [ cmake gcc automake autoconf pkg-config ]);
      }
      {
        enabled = withHy;
        emacsPkgs = epkgs: [ epkgs.hy-mode ];
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
          (pkgs.python38.withPackages (ps:
            with ps;
            let
              git-review = (when withGit [
                (ps.buildPythonPackage rec {
                  pname = "git-review";
                  version = "1.28.0";
                  doCheck = false;
                  propagatedBuildInputs = [ pbr six requests setuptools ];
                  name = "${pname}-${version}";
                  src = ps.fetchPypi {
                    inherit pname version;
                    sha256 =
                      "0nn17mfqvsa3ryjz53qjslmf60clc0vx2115kkj66h28p6vsnflf";
                  };
                })
              ]);
              daiquiri = (when withGit [
                (ps.buildPythonPackage rec {
                  pname = "daiquiri";
                  version = "2.1.1";
                  doCheck = false;
                  buildInputs = [ setuptools_scm ];
                  propagatedBuildInputs = [ python-json-logger ];
                  name = "${pname}-${version}";
                  src = ps.fetchPypi {
                    inherit pname version;
                    sha256 =
                      "1qmank3c217ddiig3xr8ps0mqaydcp0q5a62in9a9g4zf72zjnqd";
                  };
                })
              ]);
              git-pull-request = (when withGit [
                (ps.buildPythonPackage rec {
                  pname = "git-pull-request";
                  version = "6.0.1";
                  doCheck = false;
                  buildInputs = [ setuptools_scm ];
                  propagatedBuildInputs = [ PyGithub daiquiri attrs ];
                  name = "${pname}-${version}";
                  src = ps.fetchPypi {
                    inherit pname version;
                    sha256 =
                      "1465bzbvc1c87xj5n0cvnx2srdgq8kjj461yzwz86lw6fkfc053a";
                  };
                })
              ]);
              typing-extensions = ps.typing-extensions.overridePythonAttrs
                (old: rec {
                  version = "3.7.4.3";
                  src = ps.fetchPypi {
                    inherit version;
                    pname = "typing_extensions";
                    sha256 =
                      "0356ljrrplm917dqgpn8wjkw6j3mpp916gwxas7jhc3xc4xhgm4r";
                  };
                });
              black = ps.black.overridePythonAttrs (old: rec {
                propagatedBuildInputs = [ typing-extensions ps.mypy-extensions ]
                  ++ old.propagatedBuildInputs;
                version = "20.8b1";
                doCheck = false;
                src = ps.fetchPypi {
                  inherit version;
                  pname = "black";
                  sha256 =
                    "1spv6sldp3mcxr740dh3ywp25lly9s8qlvs946fin44rl1x5a0hw";
                };
              });
              ansible = (when withAnsible [ ps.ansible ]);
              notmuch = (when withNotMuch [ ps.notmuch ]);
            in git-review ++ git-pull-request ++ ansible ++ notmuch
            ++ [ virtualenv tox pip mypy black flake8 pyyaml ]))
        ];
        emacsConfig = elisp "python";
        emacsPkgs = epkgs: [ epkgs.flycheck-mypy ];
        vscodeExtensions = vsext: [ vsext.ms-python.python ];
      }
      {
        enabled = withNix;
        buildInputs = [ pkgs.nixfmt ];
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
        enabled = withHaskell;
        name = "haskell";
        emacsConfig = elisp "haskell";
        emacsPkgs = epkgs:
          [ epkgs.haskell-mode epkgs.ormolu ]
          ++ (when withLsp [ epkgs.lsp-haskell ]);
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
        buildInputs = let
          easy-hls-src = pkgs.fetchFromGitHub {
            owner = "jkachmar";
            repo = "easy-hls-nix";
            rev = "9d64543a015563942c954b89addc1108800ed134";
            sha256 = "1szq3g34dv22fqzil549mvpdd1865s64vqnfxj0l2aw9ha32jxyz";
          };
          easy-hls =
            pkgs.callPackage easy-hls-src { ghcVersions = [ "8.10.4" ]; };
        in (with pkgs.haskellPackages; [
          ormolu
          hlint
          cabal-install
          stack
          hasktags
          ghc
        ])
        ++ (when withVSCode [ pkgs.haskellPackages.haskell-language-server ])
        ++ (when withLsp [ easy-hls ]);
      }
      {
        enabled = withRust;
        name = "rust";
        emacsPkgs = epkgs: [ epkgs.rust-mode ];
        buildInputs = [ pkgs.cargo pkgs.rustc ];
      }
      {
        enabled = withPurescript;
        buildInputs = let
          purescript = import (pkgs.fetchFromGitHub {
            owner = "justinwoo";
            repo = "easy-purescript-nix";
            rev = "fbbb27c1afd51d729939a6a2006e954dbd844846";
            sha256 = "1kw9cqycrq456dipd5mq7c1ij6jl3d9ajlnba152db3qrw5wmrg0";
          }) { pkgs = pkgs; };
        in (with purescript; [ spago purs zephyr ]);
        emacsConfig = elisp "purescript";
        emacsPkgs = epkgs: [ epkgs.purescript-mode epkgs.psci epkgs.psc-ide ];
      }
      {
        name = "nodejs";
        enabled = withReason;
        buildInputs = [ pkgs.nodejs pkgs.nodePackages.pnpm ];
      }
      {
        enabled = withReason;
        buildInputs = [ pkgs.bs-platform ] ++ (when withLsp
          [ (import ./reason-language-server.nix { pkgs = pkgs; }) ]);
        emacsPkgs = epkgs: [ epkgs.reason-mode ];
        emacsConfig = elisp "reason";
        vscodeExtensions = vpkgs:
          (pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "reason-vscode";
              publisher = "jaredly";
              version = "1.7.13";
              sha256 = "09pcvdzlk46cm5haqw58hm4bij8lwq3xf1kk9l4ny5x0gz11vs0j";
            }
            {
              name = "reasonml";
              publisher = "freebroccolo";
              version = "1.0.38";
              sha256 = "1nay6qs9vcxd85ra4bv93gg3aqg3r2wmcnqmcsy9n8pg1ds1vngd";
            }
          ]);
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
        enabled = withReasonNative;
        buildInputs = [ pkgs.ocamlPackages.reason ];
      }
      {
        name = "ocaml";
        enabled = withOcaml || withReasonNative || withReason;
        buildInputs = [
          pkgs.dune_2
          pkgs.opam
          pkgs.ocaml
          pkgs.ocamlPackages.merlin
        ];
        emacsPkgs = epkgs: [ epkgs.tuareg ];
      }
      {
        enabled = withX && withOpenGL;
        buildInputs = (when withIntel [ nixGL.nixGLIntel ]);
      }
      {
        enabled = withX && withVulkan;
        buildInputs = [
          pkgs.vulkan-loader
          pkgs.vulkan-validation-layers
          pkgs.vulkan-headers
          pkgs.vulkan-tools
        ] ++ (when withIntel [ nixGL.nixVulkanIntel ]);
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
        enabled = withJson || withYaml || withGraphQL || withCss
          || withMarkdown;
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
        buildInputs = let
          dhall = import (pkgs.fetchFromGitHub {
            owner = "justinwoo";
            repo = "easy-dhall-nix";
            rev = "aa1dafc30d36bd4609ead0faaee66e44f617f981";
            sha256 = "0n1ry6785j44kl4zp74vlvj20ik8gqh7zw9pc2g6arhh77vxhhir";
          }) { pkgs = pkgs; };
        in (with dhall;
          [ dhall-simple dhall-json-simple dhall-yaml-simple dhall-docs-simple ]
          ++ (when withLsp [ dhall-lsp-simple ]));
      }
      {
        enabled = withProtobuf;
        emacsPkgs = epkgs: [ epkgs.protobuf-mode ];
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
        enabled = withPdf;
        emacsPkgs = epkgs: [ epkgs.pdf-tools ];
      }
      {
        enabled = withOrg;
        emacsConfig = elisp "org";
        emacsPkgs = epkgs: [ epkgs.org epkgs.org-plus-contrib ];
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
        enabled = withNotMuch;
        buildInputs = with pkgs; [ notmuch msmtp dovecot_pigeonhole isync ];
        emacsPkgs = epkgs:
          [
            (epkgs.melpaBuild {
              pname = "notmuch";
              src = pkgs.notmuch.src;
              version = pkgs.notmuch.version;
              nativeBuildInputs = [ pkgs.pkg-config ];
              buildInputs = pkgs.notmuch.buildInputs;
              patches = [
                ./patches/0001-wip-add-notmuch-progressive-search-custom.patch
              ];
              recipe = pkgs.writeText "recipe" ''
                (notmuch :url "https://git.notmuchmail.org/git/notmuch" :fetcher git :files ("emacs/*.el" "emacs/*.png"))
              '';

            })
          ];
      }
      {
        enabled = withNeuron;
        buildInputs = [ (import ./neuron.nix { pkgs = pkgs; }) ];
        emacsPkgs = epkgs:
          [
            (epkgs.neuron-mode.overrideAttrs (old: {
              src = pkgs.fetchFromGitHub {
                owner = "felko";
                repo = "neuron-mode";
                rev = "d769042ca0b715c8da7947421302b52222598e95";
                sha256 = "0804ixy7q9jnw6nw8g71dx4isndca1jkwpkjb8k71ckwvvz63i17";
              };
            }))
          ];
        emacsConfig = elisp "neuron";
      }
      {
        enabled = withLsp;
        emacsConfig = elisp "lsp";
        emacsPkgs = epkgs: [ epkgs.lsp-mode epkgs.lsp-ui epkgs.lsp-ivy ];
      }
    ]);

  concatModuleText = f: builtins.concatStringsSep "\n" (map f modules);
  concatModuleList = f: builtins.concatLists (map f modules);

  # the fonts
  fonts = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.iosevka pkgs.noto-fonts-emoji ];
  };

  # the emacs derivation
  emacs-overlay = import (pkgs.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "1b2539d9e1e96fb1518a09bba53f56f5dabc5d52";
    sha256 = "0xm819cmxv0d0rm90d442a9dq59k2farwdxxqwq4gfxqqwbhmbm8";
  }) pkgs pkgs;

  emacsDrv = emacs-overlay.emacsGit;

  emacsOverride = self: super: {
    spinner = super.spinner.override {
      elpaBuild = args: super.elpaBuild (args // {
        version = "1.7.4";
        src = pkgs.fetchurl {
          url = "https://elpa.gnu.org/packages/spinner-1.7.4.tar";
          sha256 = "140kss25ijbwf8hzflbjz67ry76w2cyrh02axk95n6qcxv7jr7pv";
        };
      });
    };
  };

  emacs = ((pkgs.emacsPackagesGen emacsDrv).overrideScope' emacsOverride).emacsWithPackages (epkgs:
    (let
      # the emacs config:
      emacsSite = pkgs.writeText "default.el" ((elisp "base")
        + (elisp "base-extra") + (elisp "format-all")
        + (concatModuleText (m: m.emacsConfig)) + ''
          ;; reset gc to reasonable level
          (setq gc-cons-threshold (* 20 1024 1024))'');

      # emacs packages:
      language-id-src = pkgs.fetchFromGitHub {
        owner = "lassik";
        repo = "emacs-language-id";
        rev = "30a5bc267af7de167d0a835ead828016e6e7e14c";
        sha256 = "1wkppwh72zs8b4jqdxqpf3gikh11la03nkj8nna9bg7k8n0a4vaq";
      };
      format-all-src = pkgs.fetchFromGitHub {
        owner = "lassik";
        repo = "emacs-format-all-the-code";
        rev = "351057f7efde71dcd4b6c5eadcbcfcd8d53d2a47";
        sha256 = "1cicxsckmqh8zmad0dggsnvk72j37kdsyv0z5266ri7kb184awb4";
      };
      language-id = (epkgs.melpaBuild {
        pname = "language-id";
        src = language-id-src;
        version = "1";
        recipe = pkgs.writeText "recipe" ''
          (language-id :repo "lassik/emacs-language-id" :fetcher github :files ("*.el"))
        '';
      });
      format-all = (epkgs.melpaBuild {
        pname = "format-all";
        src = format-all-src;
        version = "1";
        packageRequires = [ language-id ];
        recipe = pkgs.writeText "recipe" ''
          (format-all :repo "lassik/emacs-format-all-the-code" :fetcher github :files ("*.el"))
        '';
      });
      inheritenv = (epkgs.melpaBuild {
        pname = "inheritenv";
        version = "0.1";
        src = pkgs.fetchFromGitHub {
          owner = "purcell";
          repo = "inheritenv";
          rev = "0.1";
          sha256 = "0ygzf70vfb7qwpsllcq5i3brprsnx3sxy2zng02mzwrr5jkx4ypc";
        };
        recipe = pkgs.writeText "recipe" ''
          (inheritenv :repo "purcell/inheritenv" :fetcher github :files ("*.el"))
        '';
      });
      ivy = [
        epkgs.smex
        epkgs.ivy
        epkgs.counsel
        epkgs.swiper
        epkgs.company
        epkgs.projectile
      ];
      prog = [ epkgs.flycheck format-all inheritenv ];
      base = [
        (pkgs.runCommand "default.el" { } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${emacsSite} $out/share/emacs/site-lisp/default.el
        '')
        epkgs.use-package
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
        epkgs.ace-window
        epkgs.ace-link
        epkgs.avy
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

  # openGL
  nixGL = import (pkgs.fetchFromGitHub
    (builtins.fromJSON (builtins.readFile ./nixGL.json))) {
      pkgs = pkgs;
      enable32bits = false;
    };

  # devenv derivations collection:
  devenv = (with pkgs; [
    bash
    fontconfig
    hack-font
    ripgrep
    man
    findutils
    glibcLocales
    pkgconfig
    systemd
    libnotify
    openssl
  ]) ++ (when withEmacs [ emacs ]) ++ (when withVim [ vim ])
    ++ (when withVSCode [ vscode ]) ++ (when withPyCharm [ pycharm ])
    ++ (concatModuleList (m: m.buildInputs));

  # share the pinned nixpkgs
  nixpkgs_src = pkgs.fetchFromGitHub
    (builtins.fromJSON (builtins.readFile ./nixpkgs.json));

  shellEnv = pkgs.mkShell {
    buildInputs = devenv;
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib/:/run/opengl-driver/lib/
      export __ETC_PROFILE_NIX_SOURCED=1
      export NIX_PATH=nixpkgs=${nixpkgs_src}
    '' + (if withAts then ''
      export ATS_LOADPATH=${pkgs.ats2}/share/emacs/site-lisp/ats2
    '' else
      "") + (if withX then ''
        export FONTCONFIG_FILE=${fonts}
      '' else
        "");
  };

in {
  devenv = devenv;
  shellEnv = shellEnv;
}
