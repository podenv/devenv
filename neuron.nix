{ pkgs ? import ./nixpkgs.nix }:

pkgs.stdenv.mkDerivation rec {
  pname = "neuron";

  version = "1.0.1.0";

  src = pkgs.fetchurl {
    url =
      "https://github.com/srid/neuron/releases/download/${version}/neuron-${version}-linux.tar.gz";
    sha256 = "0azs98pig69vv5qv2hyxnc0gr07q9f226s56gr9fqh48gv1c5l15";
  };

  propagatedBuildInputs = [ pkgs.fzf ];

  dontStrip = true;

  unpackPhase = ''
    mkdir -p $out/bin
    tar xf $src -C $out/bin
  '';

  dontInstall = true;
}
