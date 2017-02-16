[![Stories in Ready](https://badge.waffle.io/aurapm/aura.png?label=ready&title=Ready)](https://waffle.io/aurapm/aura)
[![Build Status](https://travis-ci.org/aurapm/aura.svg?branch=master)](https://travis-ci.org/aurapm/aura)
[![Join the chat at https://gitter.im/aurapm/aura](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/aurapm/aura?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

What is Aura?
=============
Aura is a package manager for Arch Linux. It's main purpose is as an
"AUR helper", in that it automates the process of installating packages
from the Arch User Repositories. It is, however, capable of much more.

The Aura Philosophy
===================
### Aura is Pacman
  Aura doesn't just mimic pacman... it _is_ pacman.
  All pacman operations and their sub-options are allowed.
  Some even hold special meaning in Aura as well.

### Arch is Arch. AUR is AUR.
  `-S` yields pacman packages and _only_ pacman packages. This agrees with
  the above. Thus in aura, the `-A` operation is introduced for obtaining
  AUR packages. `-A` comes with sub-options you're used to (`-u`, `-s`,
  `-i`, etc.).

### All together
Dependencies and packages are not built and installed one at a time.
Install order is as follows:
1. All pacman (ABS) dependencies (all at once).
2. All AUR dependencies (one at a time).
3. All AUR packages (all at once).

### Quiet Building
  By default `makepkg` output is suppressed. If you want the people
  behind you to think you're a badass hacker, then this suppression
  can be disabled by using `-x` alongside `-A`.

### Run as Root, Build as a User
  `makepkg` gets very upset if you try to build a package as root.
  That said, a built package can't be handed off to pacman and installed
  if you _don't_ run as root. Other AUR helpers ignore this problem,
  but Aura does not. Even when run with `sudo`, packages are built
  with normal user privilages, then handed to pacman and installed as root.

### Know your System
  Editing PKGBUILDs mid-build is not default behaviour.
  An Arch user should know _exactly_ what they're putting into their system,
  thus research into prospective packages should be done beforehand.
  However, for functionality's sake, the option `--hotedit` used with `-A`
  will prompt the user for PKGBUILD editing. Regardless, as a responsible
  user you must know what you are building.

### Downgradibility
  Built AUR package files are moved to the package cache.
  This allows for them to be easily downgraded when problems arise.
  Other top AUR helper programs do not do this.
  Options `--save` and `--restore` aid you during system breakage.

### No Orphans
  Sometimes dependencies lose their *required* status, but remain
  installed on your system. Sometimes AUR package "makedepends"
  aren't required at all after install. Packages like this just
  sit there, receiving upgrades for no reason.
  Aura helps keep track of and remove packages like this.

### Arch Linux for Everyone
  English is well established as the world's Lingua Franca, and is also
  the dominant language of computing and the internet. That said, it's
  natural that some people are going to be more comfortable working
  in their native language. From the beginning Aura has been built with
  multiple-language support in mind, making it very easy to add new ones.

### Haskell
  Aura is written in Haskell, which means easy developing and pretty code.
  Please feel free to use it as a [simple Haskell reference](https://github.com/aurapm/aura/blob/master/aura/doc/source/guides/hacking.rst#for-haskell-study).
  Aura code demonstrates:
  * Regexes
  * CLI flag handling
  * Monad Transformers
  * usage of Parsec

Sample Usage
============
#### New with Aura 1.2
Build a repository package manually:

    aura -M (package)

Build a repository package and all its dependencies manually:

    aura -M (package) --absdeps

Sync a single package's data to the local ABS Tree:

    aura -Mt (package)

Sync all package data in the local ABS Tree:

    aura -My

Remove only those package files from the cache that aren't present in any
package record:

    aura -Ccc

#### Installing Packages
Install an AUR package:

    aura -A (package)

Author's favourite (upgrades, removes make deps, shows PKGBUILD diffs):

    aura -Akua

Just upgrade all installed AUR packages (works with `-y`):

    aura -Au

Look up information on an AUR package:

    aura -Ai (package)

Search the AUR via a regex:

    aura -As (regex)

Display an AUR package's PKGBUILD:

    aura -Ap (package)

Display an AUR package's dependencies (and those deps' deps too):

    aura -Ad (package)

Install with makepkg output unsuppressed:

    aura -Ax (package)

Install and remove make dependencies afterwards:

    aura -Aa (package)

Install and show PKGBUILD differences:

    aura -Ak (package)

#### Working with Package Records
Store a record of all installed packages:

    aura -B

Restore a saved record. Rolls back, uninstalls, and reinstalls packages as necessary:

    aura -B --restore

#### Working with the Package Cache
Downgrade a package (this is interactive):

    aura -C (package)

Search the package cache for package files via a regex:

    aura -Cs (regex)

Backup the package cache:

    aura -Cb (/path/to/backup/location/)

Reduce the package cache to contain only 'x' of each package file:

    aura -Cc x

#### Working with the Pacman Log
Display install / upgrade history for a package:

    aura -Li (package)

Search the pacman logfile via a regex:

    aura -Ls (regex)

#### Working with Orphan Packages
Display orphan packages:

    aura -O

Adopt an orphan package:

    aura -O (package)

Uninstall all orphan packages:

    aura -Oj

More information is available in aura's manpage.

Localisation
============
As mentioned in the Philosophy above, adding new languages to Aura is
quite easy. If you speak a language other than those available and
would like it added to Aura, please consult the [Localisation Guide](https://github.com/aurapm/aura/blob/master/aura/doc/source/guides/localisation.rst)
provided.
