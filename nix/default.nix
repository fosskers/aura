let
  inherit (builtins) foldl';
  fix = unfix: let self = unfix self; in self;
  extends = overlay: unfix: self:
    let
      super = unfix self;
      selfUnmerged = overlay self super;
    in super // selfUnmerged;
  extendList = foldl' (unfix: overlay: extends overlay unfix);

commonOverlay.base = common: commonSuper: with common; {

  nixpkgs = <nixpkgs>;
  nixpkgsArg = { };
  pkgs = import nixpkgs nixpkgsArg;
  sources = import ./sources.nix { };

  # matching stack.yaml
  haskellCompiler = "ghc883"; # lts-15.13 (ghc-8.8.3)
  # pins e.g. extra-deps without hashes or revisions
  hackageSnapshot = "2020-05-19T00:00:00Z";

  haskellPackages = pkgs.haskell.packages.${haskellCompiler};
  ghc = haskellPackages.ghc;

  # stack build / stack run
  stackProject = import ./stack.nix { inherit common; };
  # "$(nix-build --no-out-link ./nix -A staticProject.fullBuildScript)"
  staticProject = import ./static.nix { inherit common; };

  static-haskell-nix = pkgs.applyPatches {
    name = "static-haskell-nix-src";
    src = sources.static-haskell-nix.outPath;
    patches = [
      ./static-haskell-nix-fixed_stack2nix-override.patch
      ./static-haskell-nix-newer-cabal-versions.patch
    ];
  };
  # lts-15+ requires Stack >= 2.0
  #   https://www.stackage.org/blog/2020/02/discontinuing-legacy-snapshots
  # You can set `haskellPackages` if you like,
  # but be careful with the compiler you move to (currently "ghc865").
  stack2nix = import (pkgs.applyPatches {
    name = "stack2nix-src";
    src = sources.stack2nix;
    patches = [
      # Make GHC base libraries dependent on GHC version.
      #   https://github.com/input-output-hk/stack2nix/pull/172
      ./stack2nix-per-ghc-base-packages.patch
      # (pkgs.fetchpatch {
      #   url = "https://github.com/input-output-hk/stack2nix/commit/c009e33af30c76b8fe94388382d816079fb5ac4e.patch";
      #   sha256 = "0x44lcmzywvzhymch0hhg4j1r3592v51xw552kawm2zd0cfp8qhz";
      # })
    ];
    postPatch = ''
      cp ${sources.ghc-utils}/library-versions/pkg_versions.txt ./pkg_versions.txt

      sed -i \
        -e '1s/ }:/, haskellPackages ? null } @ args:/' \
        -e 's/\(import .\/stack2nix.nix\) { inherit pkgs; }/\1 args/' \
        ./default.nix

      sed ./pkg_versions.txt \
        -e '/^\(#\|$\)/d; s/\(\S\+\)\/\S\+/"\1",/g; s/^\(\S\+\)\s*/  , ("\1", [/; s/,\?$/])/' | sed '1s/^  ,/ghcBaseLibsMap = M.fromListWith (++)\n  [/; $s/$/\n  ]/' \
        >> ./src/Stack2nix/External/Stack.hs
    '';
  }) { inherit pkgs; };

};

commonOverlay.pin = common: commonSuper: with common; {

  # If pinned, you'll probably want the static-haskell-nix binary subsituter.
  #   substituter: https://static-haskell-nix.cachix.org
  #   trusted-public-key: 1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU=
  pinned = true;

  nixpkgs = if pinned then sources.nixpkgs else commonSuper.nixpkgs;
  nixpkgsArg = if pinned then {
    config = { };
    overlays = [ ];
  } else commonSuper.nixpkgsArg;

};

# By patching the Nixpkgs source, patches won't be dropped due to
# reevaluations by a `pkgs.path` (not reusing the passed `pkgs` value).
# (This isn't as much of an issue for e.g. normal `<nixpkgs>` due to ambient
# mutable configuration, like `~/.config/nixpkgs/config.nix`.)
commonOverlay.patchNixpkgs = common: commonSuper: with common; {

  nixpkgsUnpatched = commonSuper.nixpkgs;
  pkgsUnpatched = import nixpkgsUnpatched nixpkgsArg;

  nixpkgs = pkgsUnpatched.applyPatches {
    name = "nixpkgs";
    src = nixpkgsUnpatched;
    patches = let
      # configure.ac: make cross-compiler detection stricter
      #   (broken since GHC 8.2.1: Fix configure detection.)
      #     https://gitlab.haskell.org/ghc/ghc/commit/18f06878ed5d8cb0cf366a876f2bfea29647e5f0
      #   https://gitlab.haskell.org/ghc/ghc/merge_requests/2234
      #   https://gitlab.haskell.org/ghc/ghc/commit/4cbd5b47a00a29b7835710f1b91bb93ac8e3f790
      nixpkgs-ghc-2234-cross-detect = [
        ./nixpkgs-ghc-2234-cross-detect.patch
      ];

      # ghc: do not use ld.gold with musl libc #84670
      #   https://github.com/NixOS/nixpkgs/pull/84741
      #   GHC does not compile on pkgsMusl after the ld.gold change
      #     https://github.com/NixOS/nixpkgs/issues/84670
      nixpkgs-84670-ghc-musl-no-ld-gold = [
        (pkgsUnpatched.fetchpatch {
          url = "https://github.com/NixOS/nixpkgs/commit/acfccf7f5ba83ce4ec279ce4416e5de5edca2fb7.patch";
          sha256 = "0iv2mbw80qjr7d7gcpv0n7j7qdpn2vxky4zimw6qf9w409pbil2f";
        })
      ];
    in
      # nixpkgs-ghc-2234-cross-detect ++
      nixpkgs-84670-ghc-musl-no-ld-gold ++
      [ ];
  };

};

in { ... } @ commonArgs: fix (extendList (common: commonArgs) [
  commonOverlay.base
  commonOverlay.pin
  commonOverlay.patchNixpkgs
  (common: commonSuper: commonArgs)
])
