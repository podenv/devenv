# devenv: a developer environment

## Overview and scope

At a high level, devenv is a modular container declared with nix.
A module defines a runtime, associated tools and IDE configuration.

For example, `withPython` results in:

- python38 with packages virtualenv, tox, pip, mypy, black
- emacs config with flycheck-mypy

`withReason` results in:

- nodejs with packages bs-platform and yarn
- emacs config with reason-mode and format-all

Vim and VSCode are also supported.

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

Use VSCode:

```
NIXPKGS_ALLOW_UNFREE=1 nix-shell --arg withEmacs false --arg withVSCode true
```

Use VIM:

```
nix-shell --arg withEmacs false --arg withVim true
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
