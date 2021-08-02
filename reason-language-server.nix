{ pkgs ? import ./nixpkgs.nix }:

pkgs.stdenv.mkDerivation rec {
  pname = "reason-language-server";

  version = "1.7.13";

  src = pkgs.fetchurl {
    url =
      "https://github.com/jaredly/reason-language-server/releases/download/${version}/rls-linux.zip";
    sha256 = "1g6xd0nclhi5qn0x983l8vza3wmn237lc3swffhnv1iylbrhr5mm";
  };

  buildInputs = [ pkgs.unzip ];

  dontStrip = true;

  unpackPhase = ''
    mkdir -p $out/
    unzip $src -d $out/
    mv $out/rls-linux $out/bin
  '';

  dontInstall = true;
}
