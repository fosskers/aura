# Migration Guide

v4 is a complete overhaul for Aura. Care has been taken to maintain prior usage
patterns, but there are some changes that the user should be aware of. If you're
new to Aura, you can skip this entirely and move straight to [Usage](usage.md).

If you notice that something other than what is mentioned here used to work but
no longer does, please [report it](https://github.com/fosskers/aura/issues).

## sudo

You no longer need to run Aura with `sudo` for "administrative" tasks. So:

```
> aura -S firefox
```

"just works", and you'll be prompted for a password as necessary. This is true
for all Aura commands.

## Package Building

### Build User and Build Directory

Since `sudo` is no longer necessary, Aura runs entirely as your personal user,
and thus `makepkg` is also invoked internally entirely as you. There are no
longer any internal user switching hacks just to build packages. This fixes a
number of historical bugs.

Aura packages were previously built in `/tmp/`, but now they are built in
`~/.cache/aura/builds/` by default. Aura also keeps its own cache, whereas
previously all built package tarballs were moved to the pacman cache under
`/var/`. Doing everything as the local user in the user's own section of the
filesystem ensures that special permissions are never necessary.

An exception to the above is when `aura` is run as the `root` user, for example
on remote servers or within Docker containers. In these cases, `root` is
detected and packages are built under `/tmp/` as the `nobody` user. Since
`nobody` has no `$HOME` and no permissions to write anywhere else, this helps
ensure that package building cannot harm your wider system.

### Makepkg output suppression with `-x`

`makepkg` output is now shown by default and cannot be hidden. `-x` is still
provided to prevent old scripts from breaking, but it has no effect.

### PKGBUILD diff viewing with `-k`

You are now prompted after the diff is printed so that you actually have a
chance to read what has changed.

### PKGBUILD Analysis

The `-P` command has been removed entirely. Further, automatic analysis that
occurred before building is now done through `shellcheck`.

## Configuration

Previously Aura was configured in `/etc/aura.conf` using a custom format. Now
configuration files exist per-user in `~/.config/aura/config.toml` and use the
TOML format. A fresh configuration file can be generated via:

```
aura conf --gen > ~/.config/aura/config.toml
```

### Locale

Aura will auto-detect your system's language from your `$LANG` variable, but
it's still possible to override this with the `language` field. Previously the
language code was just the language portion (e.g. `hi`), but now it requires the
country portion as well (e.g. `hi-IN`, Hindi from India). Acceptable values can
been found in the output of:

```
aura stats --lang
```
