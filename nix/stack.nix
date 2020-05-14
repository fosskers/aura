# Stack is unhappy without a `ghc` attribute argument
{ common ? import ./. args, ghc ? null, ... } @ args:

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
