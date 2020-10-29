# devenv: a developer environment

## Overview and scope

At a high level, devenv is a modular container declared with nix.
A module defines a runtime, associated tools and IDE configuration.

For example, `withPython` results in:

- runtime: python38
- package manager: pip, tox
- formatter and linter: black, flake8, mypy
- emacs config: flycheck-mypy
- vscode extension: ms-python

`withReason` results in:

- runtime: nodejs
- package manager: yarn
- formatter: bsrefmt
- emacs config: reason-mode
- vscode extension: reason-vscode

`withLsp` enables language-server protocol extensions.

Vim and PyCharm are also supported.

The goal is to provide the best in class developer experience for PL enthusiast.

# Documentation

Devenv documentation is organized into the following [four sections][documentation]:

[documentation]: https://www.divio.com/en/blog/documentation/

## Tutorials

### Run the container

TBD

### Install and use devenv

Starts with `nix-shell`, or using podenv:

```
podenv --x11 --network --git --home ~/.devenv nix-shell
mkdir -p ~/src/github.com/podenv/ && nix-shell -p "git" --command "git clone https://github.com/podenv/devenv ~/src/github.com/podenv/devenv"
nix-shell ~/src/github.com/podenv/devenv/
```

Use Emacs:

```
nix-shell --arg withEmacs true
```

Use ViM:

```
nix-shell --arg withVim true
```

Use VSCode:

```
NIXPKGS_ALLOW_UNFREE=1 nix-shell --arg withVSCode true
```

### Add custom module

TBD

## Howtos

### Adds python packages

TBD

### Setup hoogle code search service

TBD

### Use neuron

TBD

### Disable formatter

TBD

## Discussions

### Reproducable environment

See https://nixos.org/ or https://guix.gnu.org/

## References

### Configuration

See the [default.nix](./default.nix) entrypoint.

### Contribute

Contribution are most welcome, for example the project needs help to:

- Support more languages.
- Add Vim configuration.
- Improve documentation.
