# Package Storage Locations

This page explains all important filepaths for Aura. All paths here can [be
configured](configuration.md) in `aura.conf` or changed dynamically with CLI
flags.

## Where are packages built?

By default, non-VCS packages are built within `/tmp` in a semi-randomly named
directory. This avoids odd collisions during repeated attempts at building a
package. `/tmp` is cleared automatically upon a restart of the machine, but for
users who don't restart often, `-c`/`--clean` can be passed to `-A` to delete
these build directories proactively.

`--build` can be passed to `-A` with an **absolute** filepath to build packages
in a directory other than `/tmp`.

VCS packages (i.e. `*-git`) are built and stored in `/var/cache/aura/vcs/` in
subdirectories with fixed names. This is so that they can be reused during
upgrades; `makepkg` is smart enough to do `git pull` instead of a full `git
clone` in these cases.

`--vcspath` can be passed to `-A` to build/store such packages elsewhere.

## Where are packages stored?

Once built, all packages are sent to the Pacman cache: `/var/cache/pacman/pkg/`.

This directory can grow quite large, but can be cleaned with
[`-Cc`](downgrading.md).

## What other filepaths are there?

Aura stores historical PKGBUILDs in `/var/cache/aura/pkgbuilds/`, and saved
package set states in `/var/cache/aura/states/`.
