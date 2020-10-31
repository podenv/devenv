# A pinned nixpkgs
let
  bootstrap = import <nixpkgs> { };
  src = bootstrap.fetchFromGitHub
    (builtins.fromJSON (builtins.readFile ./nixpkgs.json));

in import src { }
