{ ... } @ commonArgs:
let common = with common; {
  pkgs = import <nixpkgs> { };
  ghc = pkgs.ghc;
  stackProject = import ./stack.nix { inherit common; };
} // commonArgs; in common
