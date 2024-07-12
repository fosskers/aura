# Package Storage Locations

This page explains all important filepaths for Aura. Build paths can [be
configured](configuration.md) in `~/.config/aura/config.toml`.

## Where are packages built?

By default, packages are built within `~/.cache/aura/builds/`. Were a package
specified in the `chroot` list in config, then it will instead be built in a
`chroot`.

## Where are packages stored?

Once built, all packages are sent to Aura's cache: `~/.cache/aura/cache/`.

This directory can grow quite large, but can be cleaned with
[`-Cc`](downgrading.md). See also `aura stats` for a view of various
Aura-related directory sizes.

## What other filepaths are there?

Aura stores clones of known AUR packages in `~/.cache/aura/packages/`, and saved
package set states in `~/.cache/aura/snapshots/`.

Also also reads your `pacman` and `makepkg` configuration, which are generally
expected to be at `/etc/pacman.conf` and `/etc/makepkg.conf` respectively, but
it will respect the usual environment variables that alter those paths.
