# WIP
{ pkgs ? import ./nixpkgs.nix }:
let devenv = import ./default.nix { pkgs = pkgs; };
in pkgs.dockerTools.buildLayeredImage {
  name = "devenv";
  contents = devenv.devenv;
}
