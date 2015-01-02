Aura Development Roadmap
========================

1.2.0.0
-------
- The Great Abstraction
- New `-M` option for building packages from the ABS.
- This will make `A` and `M` run on the same backend.
- Depedency checks will be unrestricted and beautiful.

1.3.0.0
-------
- Milestone for "legacy" Aura. Last stable version as a pacman-reliant
  AUR helper.

2.0.0.0
-------
- Aura becomes a core for multi-distro package management.
- It exposes a Hook interface for writing distro-specific install
  behaviour.
- These Hooks may or may not bolt directly to a preexisting package
  manager, as Aura 1 did.
- The preferred method is to write Haskell bindings to the libraries
  those managers use, for more direct control.
