# WIP
{ nixpkgs ? import ./nixpkgs.nix }:
let devenv = import ./default.nix { nixpkgs = nixpkgs; };
in nixpkgs.dockerTools.buildLayeredImage {
  name = "devenv";
  contents = devenv.devenv;
}
