/* nix cheat-sheet:
   # nix lang:
      https://nixos.org/manual/nix/stable/#sec-language-operators
   # Query emacs packages:
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.elpaPackages
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.melpaPackages
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.melpaStablePackages
      nix-env -f "<nixpkgs>" -qaP -A emacsPackages.orgPackages
*/

{ nixpkgs ? import ./nixpkgs.nix,
# base
withX ? true,
# editor
withEmacs ? true, withVSCode ? false, withVim ? false, withPyCharm ? false,
# lsp
withLsp ? false,
# eye friendly, low-constrat color theme
withSolarized ? true,
# emacs with vim binding
withEmacsEvil ? false,
# vscode with Vim binding
withCodeVim ? false,
# faster emacs
withEmacsGcc ? false,
# git
withGit ? true,
# notes
withNeuron ? false, withOrg ? false,
# language
withC ? true, withPython ? true, withHaskell ? false, withNix ? false,
# conf
withDhall ? true, withJson ? true, withYaml ? true,
# admin
withAnsible ? false,
# web
withGraphQL ? false, withCss ? false,
# text
withMarkdown ? true, withRestructuredText ? true,
# wip language
withIdris ? false, withOcaml ? false, withReasonNative ? false,
# javascript language
withReason ? true, withPurescript ? false,
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
      }
      {
        enabled = withGit;
        name = "git";
        minimal = true;
        emacsConfig = elisp "magit";
        emacsPkgs = epkgs: [ epkgs.magit ];
        buildInputs = [ nixpkgs.git ];
      }
      {
        enabled = withC;
        buildInputs =
          (with nixpkgs; [ cmake gcc automake autoconf pkg-config ]);
      }
      {
        enabled = withPython;
        minimal = true;
        buildInputs = [
          (nixpkgs.python38.withPackages (ps:
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
            in git-review ++ ansible
            ++ [ virtualenv tox pip mypy black flake8 ]))
        ];
        emacsConfig = elisp "python";
        emacsPkgs = epkgs: [ epkgs.flycheck-mypy ];
        vscodeExtensions = vsext: [ vsext.ms-python.python ];
      }
      {
        enabled = withNix;
        buildInputs = [ nixpkgs.nixfmt ];
        emacsPkgs = epkgs: [ epkgs.nix-mode ];
        emacsConfig = elisp "nix";
        vscodeExtensions = vsext:
          (nixpkgs.vscode-utils.extensionsFromVscodeMarketplace [
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
        buildInputs = [ nixpkgs.idris2 ];
        emacsPkgs = epkgs: [ epkgs.idris-mode ];
        emacsConfig = elisp "idris";
      }
      {
        enabled = withHaskell;
        name = "haskell";
        emacsConfig = elisp "haskell";
        emacsPkgs = epkgs: [ epkgs.haskell-mode epkgs.ormolu ];
        buildInputs = [
          (nixpkgs.haskellPackages.ghcWithHoogle
            (hpkgs: with hpkgs; [ aeson simple-cmd ]))
        ] ++ (with nixpkgs.haskellPackages; [
          ormolu
          hlint
          cabal-install
          stack
        ]);
      }
      {
        enabled = withPurescript;
        buildInputs = let
          purescript = import (nixpkgs.fetchFromGitHub {
            owner = "justinwoo";
            repo = "easy-purescript-nix";
            rev = "7b1c1635e16c7f12065db2f8ec049030fcc63655";
            sha256 = "1nybcann9aiwbvj95p6wam8xyhxwaxmfnkxmgylxcw42np2lvbzr";
          }) { pkgs = nixpkgs; };
        in (with purescript; [ spago purs purty zephyr ]);
        emacsConfig = elisp "purescript";
        emacsPkgs = epkgs: [ epkgs.purescript-mode epkgs.psci epkgs.psc-ide ];
      }
      {
        name = "nodejs";
        enabled = withReason;
        buildInputs =
          [ nixpkgs.nodejs nixpkgs.yarn nixpkgs.nodePackages.parcel-bundler ];
      }
      {
        enabled = withReason;
        buildInputs = [ nixpkgs.bs-platform ];
        emacsPkgs = epkgs: [ epkgs.reason-mode ];
        vscodeExtensions = vpkgs:
          (nixpkgs.vscode-utils.extensionsFromVscodeMarketplace [
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
        enabled = !withEmacsEvil;
        emacsPkgs = epkgs: [ epkgs.anzu ];
        emacsConfig = elisp "anzu";
      }
      {
        enabled = withReasonNative;
        buildInputs = [ nixpkgs.ocamlPackages.reason ];
      }
      {
        name = "ocaml";
        enabled = withOcaml || withReasonNative;
        buildInputs = [
          nixpkgs.dune_2
          nixpkgs.opam
          nixpkgs.ocaml
          nixpkgs.ocamlPackages.merlin
        ];
        emacsPkgs = epkgs: [ epkgs.tuareg ];
      }
      {
        enabled = withJson;
        emacsConfig = elisp "json";
      }
      {
        enabled = withYaml;
        emacsConfig = elisp "yaml";
        emacsPkgs = epkgs: [ epkgs.yaml-mode ];
      }
      {
        enabled = withJson || withYaml || withGraphQL || withCss
          || withMarkdown;
        name = "prettier";
        buildInputs = [ nixpkgs.nodePackages.prettier ];
      }
      {
        enabled = withDhall;
        name = "dhall";
        emacsConfig = elisp "dhall";
        emacsPkgs = epkgs: [ epkgs.dhall-mode ];
        vimConfig = "let g:dhall_format=1";
        vimPkgs = vpkgs: [ vpkgs.dhall-vim ];
        buildInputs = let
          dhall = import (nixpkgs.fetchFromGitHub {
            owner = "justinwoo";
            repo = "easy-dhall-nix";
            rev = "3e9101c5dfd69a9fc28fe4998aff378f91bfcb64";
            sha256 = "1nsn1n4sx4za6jipcid1293rdw8lqgj9097s0khiij3fz0bzhrg9";
          }) { pkgs = nixpkgs; };
        in (with dhall;
          [ dhall-simple dhall-json-simple dhall-yaml-simple ]
          ++ (when withLsp [ dhall-lsp-simple ]));
      }
      {
        enabled = withMarkdown;
        emacsConfig = elisp "markdown";
        emacsPkgs = epkgs: [ epkgs.markdown-mode ];
      }
      {
        enabled = withOrg;
        emacsConfig = elisp "org";
        emacsPkgs = epkgs: [ epkgs.org ];
      }
      {
        enabled = withNeuron;
        buildInputs = [ (import ./neuron.nix { nixpkgs = nixpkgs; }) ];
        emacsPkgs = epkgs:
          [
            (epkgs.neuron-mode.overrideAttrs (old: {
              src = nixpkgs.fetchFromGitHub {
                owner = "felko";
                repo = "neuron-mode";
                rev = "d769042ca0b715c8da7947421302b52222598e95";
                sha256 = "0804ixy7q9jnw6nw8g71dx4isndca1jkwpkjb8k71ckwvvz63i17";
              };
            }))
          ];
        emacsConfig = elisp "neuron";
      }
    ]);

  concatModuleText = f: builtins.concatStringsSep "\n" (map f modules);
  concatModuleList = f: builtins.concatLists (map f modules);

  # the fonts
  fonts = nixpkgs.makeFontsConf { fontDirectories = [ nixpkgs.hack-font ]; };

  # the emacs derivation
  emacs-overlay = import (nixpkgs.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "6df62227999e980e04700eb4078b7bb1d92f6db7";
    sha256 = "0hj17qm7z9q73wwwxxj31p1hg72fr89wkp7qxz82336kkjp0r30c";
  }) nixpkgs nixpkgs;

  emacsDrv = if withEmacsGcc then emacs-overlay.emacsGcc else nixpkgs.emacs;

  emacs = (nixpkgs.emacsPackagesGen emacsDrv).emacsWithPackages (epkgs:
    (let
      # the emacs config:
      emacsSite = nixpkgs.writeText "default.el" ((elisp "base")
        + (concatModuleText (m: m.emacsConfig)) + ''
          ;; reset gc to reasonable level
          (setq gc-cons-threshold (* 20 1024 1024))'');

      # emacs packages:
      language-id-src = nixpkgs.fetchFromGitHub {
        owner = "lassik";
        repo = "emacs-language-id";
        rev = "4bfda9f6351f8327024551c20fe882384941214b";
        sha256 = "0gh1zn767gxs0dkl04yrcwldfkalf24azj9bhd3xb3bc1m8xlqsr";
      };
      format-all-src = nixpkgs.fetchFromGitHub {
        owner = "lassik";
        repo = "emacs-format-all-the-code";
        rev = "361178827723cb11ca774431c938c27894a08e13";
        sha256 = "0dli6kg68hh2w0amfw51f50g6za7qavg33nmxlf8m9q9f5c1fs0h";
      };
      language-id = (epkgs.melpaBuild {
        pname = "language-id";
        src = language-id-src;
        version = "1";
        recipe = nixpkgs.writeText "recipe" ''
          (language-id :repo "lassik/emacs-language-id" :fetcher github :files ("*.el"))
        '';
      });
      format-all = (epkgs.melpaBuild {
        pname = "format-all";
        src = format-all-src;
        version = "1";
        packageRequires = [ language-id ];
        recipe = nixpkgs.writeText "recipe" ''
          (format-all :repo "lassik/emacs-format-all-the-code" :fetcher github :files ("*.el"))
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
      prog = [ epkgs.flycheck format-all ];
      base = [
        (nixpkgs.runCommand "default.el" { } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${emacsSite} $out/share/emacs/site-lisp/default.el
        '')
        epkgs.use-package
        epkgs.rainbow-delimiters
        epkgs.undo-tree
        epkgs.diff-hl
        epkgs.dash
        epkgs.dash-functional
        epkgs.f
        epkgs.simpleclip
        epkgs.diminish
      ] ++ ivy ++ prog;
      lsp = [ epkgs.lsp-mode epkgs.lsp-ui epkgs.company-lsp ];
    in base ++ (when withLsp lsp)
    ++ (concatModuleList (m: m.emacsPkgs epkgs))));

  # vim
  vim = nixpkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.packages.myVimPackage = {
      # loaded on launch
      start = concatModuleList (m: m.vimPkgs nixpkgs.vimPlugins);
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
  vscode = nixpkgs.vscode-with-extensions.override {
    vscodeExtensions =
      (when withCodeVim [ nixpkgs.vscode-extensions.vscodevim.vim ])
      ++ (concatModuleList (m: m.vscodeExtensions nixpkgs.vscode-extensions));
  };

  # pycharm
  pycharm = nixpkgs.jetbrains.pycharm-community;

  # devenv derivations collection:
  devenv = (with nixpkgs; [ bash fontconfig hack-font ripgrep man findutils ])
    ++ (when withEmacs [ emacs ]) ++ (when withVim [ vim ])
    ++ (when withVSCode [ vscode ]) ++ (when withPyCharm [ pycharm ])
    ++ (concatModuleList (m: m.buildInputs));

  shellEnv = nixpkgs.mkShell {
    buildInputs = devenv;
    shellHook = ''
      export FONTCONFIG_FILE=${fonts}
      export LD_LIBRARY_PATH=${nixpkgs.stdenv.cc.cc.lib}/lib/:/run/opengl-driver/lib/
    '';
  };

in {
  devenv = devenv;
  shellEnv = shellEnv;
}
