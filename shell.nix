{ common ? import ./nix args, ... } @ args:

let
  inherit (common) pkgs stackProject;
in
pkgs.mkShell {
  name = "aura-shell";
  inputsFrom = [ stackProject ];
}
