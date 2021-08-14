# A custom nix expression to set the devenv options
# This expression accepts two arg, withX and withRuntime
{ withX ? false, withRuntime ? true }:
let
  modules = {
    withEmacs = true;
    withGit = true;
    withLsp = true;
    withX = withX;
    withShake = true;
    withHaskell = true;
    withRust = true;
    withGo = true;
    withPurescript = true;
    withNix = true;
    withRescript = true;
    withTypescript = true;
    withPython = true;
    withRpm = true;
    withDhall = true;
    withNeuron = true;
    withGLSL = true;
    withProtobuf = true;
    withPlantuml = true;
    withOrg = true;
    withRest = true;
    withGraphQL = true;
  };
in (import ./default.nix (modules // { withRuntime = withRuntime; })).devenv
