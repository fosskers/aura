# Run using:
#
#     $(nix-build --no-link -A fullBuildScript)
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  cabalPackageName = "aura";
  compiler = "ghc865"; # matching stack.yaml

  # Pin nixpkgs version.
  pkgs = import (fetchTarball https://github.com/nh2/nixpkgs/archive/a2d7e9b875e8ba7fd15b989cf2d80be4e183dc72.tar.gz) {};

  # Pin static-haskell-nix version.
  static-haskell-nix = fetchTarball https://github.com/nh2/static-haskell-nix/archive/1d37d9a83e570eceef9c7dad5c89557f8179a076.tar.gz;

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = "2019-06-11T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    set -x
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

  static_haskellPackages = static-stack2nix-builder.haskell-static-nix_output.haskellPackages;

  # Override some packages in the snapshot.
  static_haskellPackages_with_fixes = static_haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {

      # Remove nixpkgs-specific patch that no longer applies to >= 0.16
      # See https://github.com/NixOS/nixpkgs/blob/a2d7e9b875e8ba7fd15b989cf2d80be4e183dc72/pkgs/development/haskell-modules/configuration-nix.nix#L473-L474
      servant-client-core = pkgs.haskell.lib.overrideCabal super.servant-client-core (old: {
        patches = with pkgs.lib;
          filter (p: !(hasSuffix "streamBody.patch" p)) (old.patches or []);
      });

    });
  });
in
  {
    static_package = static_haskellPackages_with_fixes."${cabalPackageName}";
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
