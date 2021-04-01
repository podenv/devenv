{ trivialBuild, magit, fetchgit, hub }:
trivialBuild {
  pname = "mpr";
  src = fetchgit {
    url = "https://github.com/chmouel/emacs-mpr.git";
    sha256 = "1myfsbi9f68263gw6hk1yz08y5jhjcf8iviq2va292xd69nfic1q";
    rev = "9229b21474d3084562b0adb90525cb82cdf7c35d";
  };
  propagatedBuildInputs = [ hub ];
  buildInputs = [ magit ];
  version = "1";
}
