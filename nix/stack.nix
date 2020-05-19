# Stack is unhappy without a `ghc` attribute argument
{ ghc ? null, common ? import ./. args, ... } @ args:

let
  inherit (common) pkgs ghc;
in
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "auraEnv";
  buildInputs = [
    pkgs.zlib
  ];
}
