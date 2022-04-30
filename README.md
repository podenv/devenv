# devenv: a developer environment

## Overview and scope

At a high level, devenv is a modular IDE configuration declared with Nix.
It comes in three flavors:

- minimal: just git and basic IDE configuration
- normal: common languages
- complete: includes support for all the languages

And it supports four editors:

- emacs
- emacs-nox
- vim
- vscode

## Use

Run the editor with the normal flavor:

- `nix run github:podenv/devenv#emacs-nox`
- `nix run github:podenv/devenv#vim`
- `nix run github:podenv/devenv#vscode`

Install the languages toolchains:

- `nix profile install github:podenv/devenv#emacs-nox $(nix run github:podenv/devenv#toolchains)`

To use the other flavors, add `-minimal` or `-extra` to the attribute name, for example:

- `nix run github:podenv/devenv#vim-minimal`
- `nix run github:podenv/devenv#emacs-complete`

Install all the runtimes with:

```ShellSession
$ nix profile install github:podenv/devenv#emacs-complete $(nix run github:podenv/devenv#toolchains-complete)
```

## Inspect

Get the list of installables:

```ShellSession
$ nix flake show github:podenv/devenv
```

Get the dependencies list:

```ShellSession
$ nix path-info --derivation -rsSh $installable
```

> Try running `nix build $installable` if path-info fails with `error: path '/nix/store/...` is not valid

Get the runtime dependencies list:

```ShellSession
$ nix path-info -rsSh $installable
```

Display the list as a tree:

```ShellSession
$ nix-store -q --tree $(nix path-info $installable)
```

## Contribute

Contribution are most welcome, for example the project needs help to:

- Support more languages.
- Add Vim configuration.
- Improve documentation.
- Define more installables (such as `emacs-evil`).
