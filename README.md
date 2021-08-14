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

`withRescript` results in:

- runtime: nodejs
- package manager: yarn
- formatter: rescript
- emacs config: reason-mode

`withLsp` enables language-server protocol extensions.

Vim and PyCharm are also supported.

The goal is to provide the best in class developer experience for PL enthusiast.

# Documentation

Devenv documentation is organized into the following [four sections][documentation]:

[documentation]: https://www.divio.com/en/blog/documentation/

## Tutorials

### Try devenv

Prefix the following commands with `podenv --x11 --network --git --home ~/.devenv`
to use a container.

Emacs:

```
nix-shell --arg withEmacs true
```

ViM:

```
nix-shell --arg withVim true
```

VSCode:

```
nix-shell --arg withVSCode true
```

### Install devenv

Here are some useful commands to install the env in ~/.nix-profile:

```
# Using the command line:
nix-env -if ./default.nix --attr devenv --arg withVim true --arg withDhall true
# Using a custom configuration file:
nix-env -if ./emacs-demo.nix --arg withRuntime false
```

Check dependency tree before installation (replace `nix-env -if` with `nix-instantiate`):

```
nix-store -q --tree $(nix-instantiate --arg withRuntime true ./emacs-demo.nix)
```

Reset install:

```
OLD_NIX_ENV=$(readlink $(which nix-env))
nix-env -e '.*'
$OLD_NIX_ENV --arg withNix true --attr devenv -if ./default.nix
```

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
