# Aura Changelog

## Unreleased (Rust Port)

#### Added

- `-Ao` to open a foreign package's AUR page.
- `--limit` and `--reverse` as additional filters on `-As`.
- `-Cl` to print the contents of the package cache. Useful to pipe to other shell commands.
- `-Ct` to remove invalid package tarballs from the cache.
- `-Cm` to display all installed packages that are missing tarballs in the cache.
- `-Cy` to download missing tarballs for installed packages.
- `-Ci` to display data of a package's cache entries. In the case below, we see
  what versions are available to downgrade to, but also that the installed
  version is missing a tarball in the cache:

```
> aura -Ci linux
Name               : linux
Latest             : 5.8.13.arch1-1 [installed: 5.9.13.arch1-1]
Created            : 2020-10-08 08:33:38
Signature          : No
Tarball Size       : 72.09MiB
Available Versions : 5.8.13.arch1-1, 5.8.10.arch1-1
```

- `deps` command for analyzing dependency connections. Generates output in
  [Graphviz DOT format](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29),
  and can be used like so:

```
aura deps gcc --reverse --optional | dot -Tpng > graph.png
```

This produces the following image:

![](assets/gcc-graph.png)

- `open` command for opening various Aura-related webpages in your browser.
- `stats` command for viewing various data about your system:
  - `--groups (-g)`: All installed package groups.
  - `--heavy`: The Top 10 packages with the biggest installation footprint.
  - `--lang (-l)`: Available localizations and how complete they are.

#### Changed

- **Breaking:** The top-level command `--viewconf` is now called `conf`.
- **Breaking:** The top-level command `--languages` is now invoked by `stats -l`
  and also shows localization coverage for each language.
- **Breaking:** `-L` now prints to `stdout`.
- **Breaking:** `-Bc` no longer accepts an integer argument and instead clears
  all stale snapshots. "Stale" means that one or more packages in the snapshot
  is missing a tarball in the package cache.
- **Breaking:** The long form of `-Aw` has been changed from `--downloadonly` to
  `--clone`.
- **Breaking:** `--ignore` is now accepted multiple times with a single package
  argument, as opposed to the previous `--ignore=foo,bar,baz`.
- The performance of `-As` has been greatly improved.

#### Removed

- `-As --head` and `-As --tail` have been removed in favour of `--limit`.

#### Fixed

- `-Cc` now deletes associated `.sig` files as well.
