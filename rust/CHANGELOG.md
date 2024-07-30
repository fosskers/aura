# Aura Changelog

## 4.0.0 (2024-07-31)

Aura 4 represents a signicant body of work to port Aura from Haskell to Rust.
The motivations for this rewrite are [discussed here][motivation]. Overall, Aura
is now much more performant and has a 4x smaller binary.

It is no longer necessary to run `aura` with `sudo`. Aura is now internally
aware of when `sudo` is necessary and will prompt you as needed.

Aura's configuration format has also changed and it is much more customisable in
general. You can generate a new config file via:

```
aura conf --gen > ~/.config/aura/config.toml
```

Aura now builds as the local user in a local directory, which solves prior woes
involving environment variables and the sudo-barrier. However, `aura -A` can now
also be invoked by the `root` user, in which case just the actual invocation of
`makepkg` will be done as the `nobody` user. This allows Aura to be more easily
used on remote servers and within Docker containers.

For existing users, further details involving the transition from v3 to v4 are
available in the [Migration Guide][migration].

[motivation]: https://fosskers.github.io/aura/faq.html#why-did-you-rewrite-aura-in-rust
[migration]: https://fosskers.github.io/aura/migration.html

#### Added

- `-A`:
  - `-o` to open a foreign package's AUR page.
  - `-v` to look up packages by "provides" (package identities).
  - `--shellcheck` to scan PKGBUILDs before building.
  - `--limit` and `--reverse` as additional filters on `-As`.
  - Support for building in a `chroot` via `pkgctl build` if enabled in config.
- `-C`:
  - `-l` to print the contents of the package cache. Useful to pipe to other shell commands.
  - `-i` to display data of a package's cache entries. 
  - `-m` to display all installed packages that are missing tarballs in the cache.
  - `-n` to delete tarballs of packages not present in any snapshot.
  - `-t` to remove invalid package tarballs from the cache.
  - `-y` to download missing tarballs for installed packages.
- `-Oe` to display explicitly installed, top-level (i.e. unrequired) packages.
  Useful for detecting packages that you no longer need installed.
- `check` for confirming the overall health of your system.
- `conf` for generating and viewing various configuration files.
- `deps` command for analyzing dependency connections. 

```
aura deps gcc --reverse --optional --open
```

This produces the following image:

![](book/src/gcc.png)

- `free` to view information regarding software licenses of installed packages.
- `stats` for viewing various data about your system:
  - `--groups (-g)`: All installed package groups.
  - `--heavy`: The Top 10 packages with the biggest installation footprint.
  - `--lang (-l)`: Available localizations and how complete they are.
- `thanks` to view information about the people behind Aura.
- General support for `doas` across various commands.
- A proper offline info manual available via `info aura`.

#### Changed

- **Breaking:** The top-level command `--viewconf` is now called `conf`.
- **Breaking:** The top-level command `--languages` is now invoked by `stats -l`
  and also shows localization coverage for each language.
- **Breaking:** `-L` now prints to `stdout`.
- **Breaking:** `-Bc` no longer accepts an integer argument and instead clears
  all stale snapshots. "Stale" means that one or more packages in the snapshot
  is missing a tarball in the package cache.
- `-A`:
  - **Breaking:** The long form of `-Aw` has been changed from `--downloadonly` to
    `--clone`.
  - **Breaking:** `-d` used to stand for `--deps` but is now the shorthand for
    `--dryrun`. Dependency analysis can be done with the new top-level `deps`
    command.
  - **Breaking:** `--ignore` is now accepted multiple times with a single package
    argument, as opposed to the previous `--ignore=foo,bar,baz`.
  - `-Ak` now prompts you to continue after PKGBUILD diffs have been printed.
  - The performance of `-As` has been greatly improved.
  - `-Ax` now does nothing - build output is unsuppressed by default.
  - `--devel` has been renamed to `--git`, although the old name secretly still works.

#### Removed

- `-P` in its entirety.
- `-As --head` and `-As --tail` have been removed in favour of `--limit`.
- `--ignorearch`
