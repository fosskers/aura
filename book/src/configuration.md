# Configuring Aura

For certain settings we use all the time (e.g. language, build paths, etc.),
Aura is configurable via `~/.config/aura/config.toml` in the [TOML
format](https://en.wikipedia.org/wiki/TOML). This file, with sensible defaults,
can be generated via:

```
aura conf --gen > ~/.config/aura/config.toml
```

Here are the specifics of each field.

## General Settings

Governed within the `[general]` section.

| Field       | Type   | Purpose                                                         |
|:------------|:-------|:----------------------------------------------------------------|
| `cpus`      | int    | Affects parallelism in various algorithms.                      |
| `editor`    | string | The editor opened with `--hotedit`, etc.                        |
| `elevator`  | string | A sudo-like program to elevate privileges during installation.  |
| `language`  | string | A code to specify the human language of Aura's output messages. |
| `noconfirm` | bool   | Automatically accept all prompts.                               |

See `aura stats --lang` for available language codes.

## AUR Package Building 

Governed within the `[aur]` section.

| Field           | Type        | Purpose                                                      |
|:----------------|:------------|:-------------------------------------------------------------|
| `build`         | string      | A path to the build cache Aura should use.                   |
| `cache`         | string      | A path in which to store built package tarballs.             |
| `clones`        | string      | A path in which to clone package metadata.                   |
| `hashes`        | string      | A path in which to store the git hash of the latest build.   |
| `builduser`     | string      | An alternate user to build as.                               |
| `chroot`        | string list | Packages to build with `pkgctl build` in a chroot.           |
| `ignores`       | string list | Packages to never update.                                    |
| `git`           | bool        | Force update all VCS packages during `-Au`.                  |
| `hotedit`       | bool        | Prompt to edit build files (PKGBUILD, etc.) before building. |
| `shellcheck`    | bool        | Run `shellcheck` over PKGBUILDs before building.             |
| `diff`          | bool        | Display PKGBUILD diffs during upgrades.                      |
| `delmakedeps`   | bool        | Remove makedeps after building.                              |
| `clean`         | bool        | Delete a package's build directory after building.           |
| `warn_unknowns` | bool        | If `false`, suppress warnings about unknown packages.        |
| `nocheck`       | bool        | Don't run the `check()` function while building.             |
| `skipdepcheck`  | bool        | Don't perform dependency checking at all.                    |

Fields of type `string list` look like this:

```toml
ignores = ["foo", "bar", "baz"]
```

## Package Snapshots

Governed within the `[backups]` section.

| Field       | Type   | Purpose                                     |
|:------------|:-------|:--------------------------------------------|
| `snapshots` | string | A path in which to store snapshot files.    |
| `automatic` | bool   | Automatically save a snapshot during `-Au`. |
