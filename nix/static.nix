# To build:
#
# 1. Update Aura's static stack2nix output source,
#    if Cabal dependencies or stack.yaml have changed.
#
#        $(nix-build --no-out-link ./nix -A staticProject.update-stack2nix-output)
#
# 2. Build Aura's static stack2nix expression.
#
#        nix-build ./nix -A staticProject.static_package
#
# Alternatively, perform both by:
#
#     $(nix-build --no-out-link ./nix -A staticProject.fullBuildScript)
#
# Once built, the static Aura executable will be at `./result/bin/aura`.
{ stack2nix-output-path ? ./aura-stack2nix-output.nix
, common ? import ./. args, ... } @ args:
let
  cabalPackageName = "aura";
  compiler = common.haskellCompiler; # matching stack.yaml

  # Pin static-haskell-nix version.
  inherit (common) static-haskell-nix;

  # Pin nixpkgs version
  # No longer needs to be the one `stack-haskell-nix` provides,
  # as all patches have been upstreamed:
  #   https://github.com/NixOS/nixpkgs/issues/43795#issuecomment-582213348
  inherit (common) pkgs;

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    inherit (common) hackageSnapshot stack2nix;
    stack-project-dir = toString ../.; # where stack.yaml is
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  update-stack2nix-output = pkgs.writeShellScript "stack2nix-update-output.sh" ''
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    printf 'STACK2NIX_OUTPUT_PATH=%s\n' "$STACK2NIX_OUTPUT_PATH" >&2
    sed "$STACK2NIX_OUTPUT_PATH" \
      -e 's/src = '${pkgs.lib.escapeShellArg (pkgs.lib.escape ["/"] (builtins.toString (/. + ../.)))}'/src = ../' \
      > ${pkgs.lib.escapeShellArg stack2nix-output-path}
  '';

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeShellScript "stack2nix-and-build-script.sh" ''
    set -eu -o pipefail
    ${update-stack2nix-output}
    export NIX_PATH=nixpkgs=${pkgs.lib.escapeShellArg pkgs.path}
    ${pkgs.nix}/bin/nix-build \
      ${pkgs.lib.escapeShellArg ./.} -A staticProject.static_package \
      "$@"
  '';

in
  {
    static_package = static-stack2nix-builder.static_package;
    inherit update-stack2nix-output;
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
