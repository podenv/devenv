/* A pinned nixpkgs
*/
let
  bootstrap = import <nixpkgs> { };
  src =  bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "88ebc5e7ad3def65d767d347d48cbc9f069098cb";
    sha256 = "01iah5dr4k26a318x24k1k0hc2hi8jm4yi4xy5sqiiya3xc96i30";
};
in import src {}
