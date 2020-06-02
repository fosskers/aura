# Aura Changelog

## 3.1.1 (2020-06-02)

#### Changed

- Running Aura with `sudo -E aura ...` will ensure that the transfer of the true
  user's environment will persist all the way to the internal `makepkg` calls.
  This should help people who have set custom paths for GPG, `npm`, etc., via
  environment variables. See [#606](https://github.com/fosskers/aura/issues/606).
  Users with standard setups don't need to worry about `-E`.

#### Removed

- Explicit dependency on `microlens`. Everything Aura uses now comes through `rio`.

## 3.1.0 (2020-05-27)

#### Added

- The `--skipdepcheck` flag to skip all dependency solving. Combine this with
  `--hotedit` to avoid discrepancies in custom dependency listings.
- `--skippgpcheck` is now available to pass through to `makepkg`.
- `aura.conf` now has a man page.

#### Changed

- Bumped `aeson` and `http-client` bounds.

## 3.0.0 (2020-05-20)

#### Added

- **Aura is now configurable via a conf file!** Aura expects it at
  `/etc/aura.conf`, but will not break if it's missing. If you install Aura via
  its AUR package, this file will be installed for you automatically.
- **A new top-level command: `-P`**. This allows users to analyse PKGBUILD
  files manually, as is usually done during building.
  - `-Pf` accepts a path to a PKGBUILD.
  - `-Pd` accepts a path to a directory containing a PKGBUILD.
  - `-Pa` to scan the PKGBUILDs of all locally installed AUR packages.
  - `-P` on its own will read from stdin. Combine this with `-Ap` to pull from the AUR:
```
> aura -Ap myget | aura -P

    sudo pacman -S aurvote

aura >>= sudo indicates that someone may be trying to gain root access to your machine.
aura >>= Potential PKGBUILD vulnerabilities detected.
```
- A new flag `--vcspath` to accompany the new VCS build behaviour (see below).
- A new flag `--allsourcepath` to accompany the restored `--allsource`
  functionality (see below).
- `-O --adopt` can now be called as `-Oa`.

#### Changed

- VCS packages (e.g. `*-git`, `*-svn`, etc.) and their cloned sources are now
  built and stored in `/var/cache/aura/vcs`. **Subsequent builds will no longer
  reclone everything.** [#462](https://github.com/fosskers/aura/issues/462)
- `--hotedit` will now offer to edit `.install` and `.patch` files. [#208](https://github.com/fosskers/aura/issues/208)
- Some modules have been renamed and moved around.
- `Aura.Diff` and `Aura.Pkgbuild.Base` have had their contents folded into other
  modules.

#### Fixed

- A regression that broke `-Bc`. [#592](https://github.com/fosskers/aura/issues/592)
- The functionality of `--allsource` has been restored. [#538](https://github.com/fosskers/aura/issues/538)
- A minor difference in the behaviour of `-Ss` relative to `pacman`. [#599](https://github.com/fosskers/aura/issues/599)

## 2.3.0 (2020-04-22)

#### Added

- Allow `--asdeps` to be passed to `-A` commands.
- `-y` can be passed to `-A` commands again, like `-Ayu`.
- Dutch translations. Thank you, Joris Blanken!

#### Changed

- ~15% reduction in binary size and much faster compiles due to removal of
  unnecessary dependencies.
- `-Cc` now reports how much storage space was cleared.
- A few more messages when using `--log-level=debug`.

#### Removed

- `makepkgConfFile` wasn't being used anywhere.

#### Fixed

- A bug involving multiple prompts appearing at the same time when more than one
  package needs a custom provider selection.
  [#541](https://github.com/fosskers/aura/issues/541)
- A bug involving the `+` character appearing in package names, like `crypto++`.
  [#520](https://github.com/fosskers/aura/issues/520)
- A regression where the name of a parent package wouldn't be displayed when a
  dependency couldn't be found.
  [#513](https://github.com/fosskers/aura/issues/513)

## 2.2.1 (2020-03-01)

#### Changed

- Further improved Italian translations.

#### Fixed

- Pass the Pacman flag `--overwrite` through properly.

## 2.2.0 (2020-02-25)

#### Added

- `--log-level` flag. Setting this to `debug` will give you some verbose logging
  output. This is different from the usual `-x` behaviour.

#### Changed

- Updated Italian translations. Grazie, Cristian Tentella!
- Support for GHC 8.8.2.

#### Fixed

- Users with many AUR packages installed will no longer see mysterious AUR
  connection failures. ([#528](https://github.com/fosskers/aura/issues/528))

## 2.1.0 (2020-02-17)

#### Added

- Reinstated `-Aw`, which downloads a snapshot tarball of an AUR package.

## 2.0.6 (2020-02-16)

#### Fixed

- Fixed the broken `-S`.

## 2.0.5 (2020-02-16)

#### Fixed

- Fixed a bug that prevented `-Syuu` and `-Scc`.

## 2.0.4 (2020-02-08)

#### Changed

- Removed `fused-effects` dependency in favour of `rio` to simplify code.

## 2.0.3

#### Changed

- Updated Spanish translations. Thanks to Max Ferrer!

#### Fixed

- Bug #543 involving locales. Thanks to Alexey Kotlyarov!

## 2.0.2

- Bug fixes and performance improvements.

## 2.0.0

This is a large update representing about a month of full-time effort. Aura is now
*much* faster, solves dependencies more reliably, has a few new features, and
many fewer bugs. This is all while modernizing the code and seeing a ~15% decrease
in overall code size.

### Improvements

#### Dependency Handling

- Dependency resolution is now much faster and **handles split packages correctly**.
  As such, the following troublesome packages now build correctly:
  - `android-sdk`
  - `backintime`
  - `clion`
  - `libc++`
  - `mysql-connector-c++`
  - `telegram-desktop-dev`
  - `zoom`
- Dependency provider selection for AUR packages.
  - Example: `cron` is a legal dependency to specify, but there exists no package
    with that name. `cronie` and `fcron` both "provide" `cron`, and now the user
    can manually make a selection.
  - Including `--noconfirm` will have Aura make its best guess.
- If the exact version of an AUR package is available in the package cache, it
  will be used automatically instead of being rebuilt. You can instead force a
  rebuild with `--force`.

#### PKGBUILD Analysis

- In light of the recent [compromise of the Acroread package](https://lists.archlinux.org/pipermail/aur-general/2018-July/034151.html),
  Aura nows performs static PKGBUILD analysis before building, and warns the user if
  potentially malicious terms like `curl` are found.
  - This feature can be disabled with `--noanalysis`. Caveat emptor!
  - **This feature is a supplement in checking PKGBUILD safety, not a fool-proof replacement.**
    It is always your responsiblity to understand what build scripts are running
    on your machine.

#### Saved Package State

- `-Su` and `-Au` automatically save a package state before updating (unless you're doing `--dryrun`).
  This lets you more easily roll back from problematic updates.
- Saved package states can now be "pinned", which will protect them from removal via `-Bc`.
  To pin a certain state, open its JSON file (see below in *Breaking Changes*) and edit the
  `pinned` field from `false` to `true`.

#### CLI Flags

- Various CLI flag improvements:
  - `-A --json <packages>`. Query the AUR directly for a package's raw JSON data. Great for debugging.
  - `-Br` has been restored as short-hand for `-B --restore`.
  - Added `-Bl` to list all saved package state filenames.
  - `-Cb` added as a short-hand for `-C --backup`.
  - The Pacman flags `--ignoregroup`, `--cachedir`, `--config`, and `--logfile` also now affect Aura.
  - `--dryrun` no longer requires sudo.
  - `--color never` turns off all text colouring. Further, by default, Aura will
    only automatically colour text when it detects that the output device is a terminal
    (and not a Unix pipe, say). These behaviours match Pacman.

#### Translations

- Improved Japanese translations thanks to **Onoue Takuro**.
- Improved Portuguese translations thanks to **Wagner Amaral**.
- Improved Russian translations thanks to **Alexey Kotlyarov**.

#### Misc.

- Packages that aren't interdependent will be built in succession without prompting
  the user, only calling down to `pacman` once per group.
- Modernized the Haskell code:
  - Removed custom CLI flag handling in favour of `optparse-applicative`.
  - Removed custom package version number parsing in favour of `versions`.
  - Removed custom text colouring code in favour of `prettyprinter`.
  - Removed the `Aura` Monad in favour of Extensible Effects via `freer-simple`.
  - Removed custom shell interaction code in favour of `typed-process`.
  - Used `async` to make AUR and `pacman` calls concurrent.
  - `megaparsec` parsers used in place of hacky Regexes.
- `aura` is now a library as well, and can be pulled into other Haskell projects.

### Breaking Changes

- `-B` now saves package states as JSON. This makes them readable by other tools,
  and also improves internal code quality. **All old package state files are no longer readable by Aura.**
  - The `"time"` field in these files is now a Haskell `ZonedTime`.
- Various CLI flag changes:
  - `--auradebug` is now just `--debug`, matching Pacman.
  - `--aurignore` is now just `--ignore`, matching Pacman.
  - `-Aw` has been removed.
  - `-y` no longer works with `-A`. Perform an `-Sy` ahead of time instead.
  - `-O` no longer accepts arguments to adopt packages, it only displays current
    orphans. Use `-O --adopt` instead for the old behaviour.
  - `-Ccc` is now `-C --notsaved`.
- Help messages (`-h`) are no longer localised.
- Support for `powerpill` removed.
- Support for `customizepkg` removed.

### Bug Fixes

- Aura no longer returns an exit code of 1 if no packages are available to upgrade.
- `-Aq` no longer fails at the package installation step.
- Ctrl+C at certain moments no longer preserves the Pacman lock file.
- `makepkg` output is no longer coloured green.

## 1.4.0
- *Dependency resolution vastly improved.* We removed the Bash parser that used
  to poorly handle the bulk of this.
- Chinese translations thanks to Kai Zhang.
- `-M` operator and associated code fully removed.

## 1.3.9
- Updated Swedish translations
- Disabled `-M` operator due to the `abs` tool being deprecated by Arch Linux

## 1.3.8
- Fixed behaviour of `-B` flags. For restoring of saved states, use the long
  form: `aura -B --restore`. Cache backups also need to take their long form: `aura -C --backup`.
- Fixed handling of language flags. Thanks to Doug Patti!

## 1.3.5
- Aura now uses version 5 of the `aur` package, to fix a critial bug
- Updated Spanish and Polish

## 1.3.4
- Bash parser bug fix. Fixes some packages.

## 1.3.3
- Bash parser extended to be able to handle bash array expansions.
  This enables packages with more (Bash-wise) complex PKGBUILDs to build
  properly.

## 1.3.2.1
- `-Ai` and `-As` show popularity values.
- `aur4` is no longer referenced.
- `Yes/No` prompts are now localized.
- Aura can be built with `stack`.
- Updated German translation.

## 1.3.1.0
- Aura builds against GHC 7.10.
- Updated German and Russian translations.

## 1.3.0.4
- Must use `--builduser` when building as root.
- Bug fix regarding `--needed`.
- Updated Portuguese translation.

## 1.3.0.3
- Pacman flags `--ignore` and `--ignoregroup` now work.
- Bug fixes.

## 1.3.0.2
- (Bug fix) If a user tries to install a package in `IgnorePkg`, they
  will now be prompted.
- Man page updated.
- Dependencies updated.

## 1.3.0.1
- (Bug fix) Tarballs are now downloaded from a URL provided by the RPC.

## 1.3.0.0
- Last major version of Aura 1! We have entered the design phase for Aura 2,
  the implementation of which will transform Aura into a multi-distro
  package management platform.
- Aura 1 itself has entered "legacy" mode. The only releases to be made
  on Aura 1 after this will be of `1.3.0.x`. You'll likely never see
  `1.3.1.x`.
- Befitting a major release, we have:

  - New AUR interaction layer via the `aur` package. This fixes nasty
    "AUR lookup failed" errors.
  - `http-conduit` dropped for `wreq`, which is much easier to use.
  - Better version number parsing/comparison on installation/upgrading.
  - Package state backups have had their format changed. This BREAKS _all_
    previously saved states. Please delete your old ones!
  - Implemented extended `--needed` functionality for the AUR side of Aura.
    AUR packages won't build if they're already installed.
  - Indonesian translations!
  - Other updated translations.

## 1.2.3.4
- zsh completions completely redone (thanks to Sauyon Lee!)
  Having `aur-git` installed will let you auto-complete on AUR packages.

## 1.2.3.3
- `-As --{head,tail}` can now be passed numbers to truncate the results
  to any number you want. The default is 10.
- Updated Russian translation.

## 1.2.3.2
- Expanded Bash completions:
    Aura Only
      * Expanded completion for all options and search sub-options
      * Package completion for -M/--abssync
      * Completion for orphans using self-generated list
    Pacman
      * Include completion for all pacman options
      * Directory or file completion for some common options
- Use `--dryrun` with `-A` and `-M` install options to test everything
  up until actual building would occur (dependency checks, etc.)

## 1.2.3.1
- Network.HTTP.Conduit errors are now caught properly
  and don't crash aura.
- `customizepkg` usage corrected.
- zsh completions slightly expanded.

## 1.2.3.0
- Moved to `Network.HTTP.Conduit` from `Network.Curl`
  This fixes the AUR connection issues.
  Binary size has increased by quite a bit.

## 1.2.2.1
- `-Ai` now shows dependencies.

## 1.2.2.0
- Happy New Year!
- makepkg's `--ignorearch` flag is now visible to Aura.
  This allows users to build AUR packages on ARM devices
  without worrying about architecture restrictions in PKGBUILDs.
- Use `--head` and `--tail` to truncate `-As` results.
- `-B` now uses local time.
- Bug fixes and translation updates

## 1.2.1.3
- `-As` results now sort by vote. Use `--abc` to sort alphabetically.
- "[installed]" will now be shown in `-As` results if you have it.
- Fixed Bash parsing bug involving `\\` in arrays
- Fixed broken `-C`
- Updated Italian translation
- Updated French translation

## 1.2.1.2
- Happy Canadian Thanksgiving
- Bug fixes

## 1.2.1.1
- Norwegian translation added!
- Dependency checks slightly faster
- `--hotedit` and `--custom` can now be used together
- Bug fixes

## 1.2.1.0
- New `builduser` option
- `Prelude.head` bug fixed
- Dependency checking is faster
- New `-k` output
- `--absdeps` works properly now
- Other bug fixes

## 1.2.0.2
- Bug fixes and spelling corrections.

## 1.2.0.1
- Fixes dependency build order bug.

## 1.2.0.0
- New operator `-M` for building ABS packages. Has its own family of options.
- Pre-built binary package available (x86_64 only)
- Updates to Aura are now prioritized like pacman updates.
- Dependency checking is now faster.
- Use `-Ccc` to clean the cache of only packages not saved in any package
  record.
- `-Ai` now shows Maintainer name.
- Extensive API changes.

## 1.1.6.2
- New option `--no-pp`. Disables use of powerpill, even if you have it.
- This is a light release, as major work is being done on version 1.2 on
  another development branch.

## 1.1.6.1
- Compatable with pacman 4.1
- All pacman-color support removed
- `-As` output slightly altered to match pacman.
- Bug fixes.

## 1.1.6.0
- New option `--build` for specifying AUR package build path.
- Vote number now shown in `-As` output.
- Fixed Repo/AUR name collision bug.
- API Change: Argument order for functions in `Aura/Languages` changed.

## 1.1.5.0
- `customizepkg` now usable with Aura.
  Activate with the `--custom` option.
- API Change: Aura/Pkgbuilds now a set of libraries as Aura/Pkgbuild/*

## 1.1.4.3
- Fixed flaw in `-Br`.
- Fixed repititious `-Ad` output.
- API Change: Aura/AurConnection renamed to Aura/AUR
- API Change: function names in Aura/Languages now have better names.

## 1.1.4.2
- Haskell deps have been moved back to `makedepends`.
- haskell-http removed as dependency.
- API Change: function naming conventions in `Aura/Languages.hs` has been
  changed. The localisation guide was also updated to reflect this.

## 1.1.4.1
- Support for the $LANG environment variable.
- Aura will now pause before post-build installation if the package database
  lock exists. This means you can run multiple instances of Aura and avoid
  crashes.

## 1.1.4.0
- Serbian translation added. Thank you, Filip Brcic!
- Fixed bug that was breaking `aura -Ss`.

## 1.1.3.0
- Changed `--save` and `--restore` to `-B` and `-Br`.
  `--save` is now just an alias for `-B`, but `--restore`
  must be used with `-B`.
- New option `-Bc` for removing old unneeded package states.
- `-Br` output is now sorted better and makes more sense.
- Bash Parser can now properly parse `if` blocks, meaning packages
  that have conditional dependencies based on architecutre will now
  build properly.
- API Change: `Aura.General` is now `Aura.Core`
- Dep Change: `haskell-url` no longer needed.

## 1.1.2.1
- Added message to `--save`.

## 1.1.2.0
- Bash parser completely rewritten.
- Bug fixes (thanks to the new parser)

## 1.1.1.0
- New option `--devel`. Rebuilds all devel packages installed.
- Italian translation added! Thank you Bob Valantin!
- Support for `powerpill` added. It will be used if installed, unless
  the PACMAN variable is specifically set to something different.
- Aura can now handle PKGBUILDs that produce multiple .pkg.tar files.
- Bug fixes

## 1.1.0.0
- New `--save` and `--restore` options.
- New option `-Ak` for showing PKGBUILD diffs when upgrading.
- New option `--aurignore` for ignoring AUR packages.
- Aura now reads `color.conf`.
- Massive breaking API changes everywhere.
- Aura now runs on the Aura Monad.
- Code is quite cleaner now.

## 1.0.8.1
- Bash completions added.
- zsh completions added.
- Changed `--conf` to `--viewconf`
- Fixed bug involving "symlink" Haskell error.

## 1.0.8.0
- Moved certain general functions to `Aura.Utils`
- Moved `-L`, `-O`, `-A` functions out of `aura.hs`.
- `--hotedit` functionality altered (fix).
- The license message is now more badass.

## 1.0.7.0
- New libraries: Aura.Time, Aura.State
- Moved `-C` functionality to `Aura.C`
- New secret option you don't get to find out about until 1.1
- Fixed manually alignment stupidity with `-Li`.
- Bug fixes

## 1.0.6.0
- New libraries: ColourDiff, Data.Algorithm.Diff, Aura.Pkgbuilds
- Aura.AuraLib split into Aura.General, Aura.Build, Aura.Dependencies
- New secret option you don't get to find out about until 1.1

## 1.0.5.0
- Fixed bug where packages with `+` in their name couldn't be
  searched or built.
- `-As` now allows multi-word searches, as it always should have.
- `pacman-color` integration is more complete.
  Still does not read the color.conf directly.

## 1.0.4.0
- Added French translation. Thanks to Ma Jiehong!
- Added Russian translation. Thanks to Kyrylo Silin!
- Fixed bug where packages with dots in their name wouldn't build.

## 1.0.3.2
- Moved haskell dependencies out of `makedepends` field and into
  `depends` field in PKGBUILD. Makedepends can usually be ignored
  after building, but haskell packages are a pain to rebuild
  and reregister at every build. It's more realistic to just keep
  them installed. This is what other haskell packages in the AUR
  do as well.
- Fixed pacman-color issues.

## 1.0.3.1
- Added `--auradebug` option.

## 1.0.3.0
- Compatibility with AUR 2.0 added.
- Portuguese translation added. Thanks to Henry "Ingvij" Kupty!
- Support for `pacman-color` added. Run sudo with `-E` a la:
    sudo -E aura -Ayu
- Fixed backslash parsing bug in `Bash`.

## 1.0.2.2
- Fixed parsing bug in `Bash`.
  If one package fell victim, a whole `-Au` session would fail.

## 1.0.2.1
- Added License info to source files.
- Fixed virtual package recognition bug.
- Altered version conflict error message.
- Fixed bug in Bash parser that would occasionally break parsing.

## 1.0.2.0
- Bug fixes.
- Extended the Bash parser. PKGBUILDs that had bash variables in their
  dependency arrays will now be parsed correctly.

## 1.0.1.0
- German translation (use with --german).
  Thanks to Lukas Niederbremer!
- Spanish translation (use with --spanish)
  Thanks to Alejandro Gómez!
- Replaced regex-posix with regex-pcre.
- `-As` now highlights properly.
- Moved a number of modules to `Aura/`

## 1.0.0.0
- Fixed `-V` message in terminals other than urxvt.
- New `haskell-ansi-terminal` library to do this.

## 0.10.0.0
- Internet access moved to Network.Curl library.
- `Bash.hs` library created to help with PKGBUILD parsing.
  Can currently handle string expansions a la::

    "this-is-{awesome,neat}" => ["this-is-awesome","this-is-neat"]

## 0.9.2.3
- Dependency determining speed up.
- Added AUR URL to `-Ai`.

## 0.9.3.2
- Swedish translation.
  Thanks to Fredrik Haikarainen!

## 0.9.2.0
- `-Ai` and `-As`!

## 0.9.1.0
- `-Au` is about 20 times faster.

## 0.9.?.?
- Polish translation.
  Thanks to Chris "Kwpolska" Warrick!
- Croatian translation.
  Thanks to Denis Kasak!

## 0.9.0.0
- New `-O` operation for dealing with orphan packages.
- A man page!

## 0.8.0.0
- Help message now supports multiple languages.
- Broke "no overlapping options" convention.
- `-Cz` is now `-Cb`.
- New option `-Ad`. Lists _all_ dependencies of an AUR package.
  This is to aid pre-building research.
  This option shows information you can't get from looking at PKGBUILDS!

## 0.7.3.0
- New option `--conf`. Lets you quickly view your pacman.conf.

## 0.7.2.3
- `--log` is now `-L`.
- New option `-Ls`. Search the log file via a regex.
- New option `-Li`. Reports information on a given package that has had
  any appearance in the log file.

## 0.7.0.0
- `--hotedit` option added.
- `Shell` library added.

## 0.6.0.0
- Aura passes proper exit codes to the shell upon completion.
