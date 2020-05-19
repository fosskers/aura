{ common ? import ./nix args, ... } @ args:

let
  inherit (common) pkgs sources stackProject;
in
pkgs.mkShell {
  name = "aura-shell";
  inputsFrom = [ stackProject ];
  buildInputs = [
    # for `cabal update` for matching `staticProject`'s `hackageSnapshot`
    common.haskellPackages.cabal-install
    sources.niv
  ];
}
