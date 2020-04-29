<p align="center">
  <img src="/logo/medium-150x150.png">
</p>

# The Aura Package Manager

| Build                                                              | Release                                                                                      | Chat                                                                                        | Downloads                                                               | Languages                                                                                                         |
|--------------------------------------------------------------------|----------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| ![img](https://github.com/fosskers/aura/workflows/Tests/badge.svg) | [![img](https://img.shields.io/hackage/v/aura.svg)](http://hackage.haskell.org/package/aura) | [![img](https://img.shields.io/gitter/room/aurapm/aura.svg)](https://gitter.im/aurapm/aura) | ![img](https://img.shields.io/github/downloads/fosskers/aura/total.svg) | :uk: :jp: :croatia: :sweden: :de: :es: :portugal: :fr: :ru: :it: :serbia: :norway: :indonesia: :cn: :netherlands: |

Welcome to the main repository for Aura, a secure, multilingual package manager for Arch Linux.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Aura](#aura)
    - [What is Aura?](#what-is-aura)
    - [The Aura Philosophy](#the-aura-philosophy)
        - [Aura is Pacman](#aura-is-pacman)
        - [Arch is Arch - AUR is AUR](#arch-is-arch---aur-is-aur)
        - [All Together](#all-together)
        - [Quiet Building](#quiet-building)
        - [Run as Root, Build as a User](#run-as-root-build-as-a-user)
        - [Know your System](#know-your-system)
        - [Downgradibility](#downgradibility)
        - [No Orphans](#no-orphans)
        - [Arch Linux for Everyone](#arch-linux-for-everyone)
        - [Haskell](#haskell)
    - [Installation](#installation)
        - [Prebuilt Binaries](#prebuilt-binaries)
        - [Building from Source](#building-from-source)
    - [Sample Usage](#sample-usage)
        - [Installing Packages](#installing-packages)
        - [Package Set Snapshots](#package-set-snapshots)
        - [Downgrading via the Package Cache](#downgrading-via-the-package-cache)
        - [Searching the Pacman Log](#searching-the-pacman-log)
        - [Managing Orphan Packages](#managing-orphan-packages)
    - [Localisation](#localisation)
- [The `aur` Haskell Library](#the-aur-haskell-library)
- [The `aur-security` Tool](#the-aur-security-tool)

<!-- markdown-toc end -->

# Aura

## What is Aura?

Aura is a package manager for Arch Linux. It's main purpose is as an *AUR
helper*, in that it automates the process of installating packages from the Arch
User Repositories. It is, however, capable of much more.

## The Aura Philosophy

### Aura is Pacman

Aura doesn't just mimic `pacman`; it *is* `pacman`. All `pacman` operations and
their sub-options are allowed. Some even hold special meaning in Aura as well.

### Arch is Arch - AUR is AUR

`-S` yields pacman packages and *only* pacman packages. This agrees with the
above. Thus in Aura, the `-A` operation is introduced for obtaining AUR
packages. `-A` comes with sub-options you're used to (`-u`, `-s`, `-i`, etc.).

### All Together

Dependencies and packages are not built and installed one at a time. Install
order is as follows:

1.  All pacman dependencies (all at once).
2.  All AUR dependencies that don't depend on each other (all at once, in groups).
3.  All "top level" AUR packages (all at once).

### Quiet Building

By default `makepkg` output is suppressed. If you want the people behind you to
think you're a cool hacker, then this suppression can be disabled by using `-x`
alongside `-A`.

### Run as Root, Build as a User

`makepkg` gets very upset if you try to build a package as root. That said, a
built package can't be handed off to pacman and installed if you *don't* run as
root. Other AUR helpers ignore this problem, but Aura does not. Even when run
with `sudo`, packages are built with normal user privileges, then handed to
pacman and installed as root.

### Know your System

Editing PKGBUILDs mid-build is not default behaviour. An Arch user should know
*exactly* what they're putting into their system, thus research into prospective
packages should be done beforehand. However, for functionality's sake, the
option `--hotedit` used with `-A` will prompt the user for PKGBUILD editing.
Regardless, as a responsible user you must know what you are building.

### Downgradibility

Built AUR package files are moved to the package cache. This allows for them to
be easily downgraded when problems arise. Other top AUR helper programs do not
do this. The option `-B` will save a package state, and `-Br` will restore a
state you select. `-Au` also automatically invokes a save, to help you roll back
from problematic updates.

### No Orphans

Sometimes dependencies lose their *required* status, but remain installed on
your system. Sometimes AUR package `makedepends` aren't required at all after
install. Packages like this just sit there, receiving upgrades for no reason.
Aura helps keep track of and remove packages like this.

### Arch Linux for Everyone

English is well established as the world's Lingua Franca, and is also the
dominant language of computing and the internet. That said, it's natural that
some people are going to be more comfortable working in their native language.
From the beginning Aura has been built with multiple-language support in mind,
making it very easy to add new ones.

### Haskell

Aura is written in Haskell, which means easy development and beautiful code.
Please feel free to use it as a simple Haskell reference. Aura code
demonstrates:

-   Parser Combinators (`megaparsec`)
-   CLI Flag Handling (`optparse-applicative`)
-   Concurrency (`scheduler`)
-   Shell Interaction (`typed-process`)
-   Pretty Printing (`prettyprinter`)
-   Logging (`rio`)

## Installation

### Prebuilt Binaries

It is recommended to install the prebuilt binary of Aura:

```bash
git clone https://aur.archlinux.org/aura-bin.git
cd aura-bin
makepkg
sudo pacman -U <the-package-file-that-makepkg-produces>
```

### Building from Source

You will need the [Stack Tool](https://docs.haskellstack.org/en/stable/README/)
for Haskell to compile Aura yourself. Then:

```bash
git clone https://github.com/fosskers/aura.git
cd aura
stack install -- aura
```

This may take a while to initially build all of Aura's dependencies. Once
complete, your `aura` binary will be available in `/home/YOU/.local/bin/`.

## Sample Usage

### Installing Packages

| Command              | Function                                                                              |
|----------------------|---------------------------------------------------------------------------------------|
| `aura -A <package>`  | Install an AUR package.                                                               |
| `aura -Au`           | Upgrade all installed AUR packages.                                                   |
| `aura -Akuax`        | Author's favourite (upgrades, removes makedeps, shows PKGBUILD diffs, shows progress) |
| `aura -Ai <package>` | Look up information on an AUR package.                                                |
| `aura -As <regex>`   | Search the AUR via a regex.                                                           |
| `aura -Ap <package>` | Display a package's PKGBUILD.                                                         |

### Package Set Snapshots

| Command        | Function                                                        |
|----------------|-----------------------------------------------------------------|
| `aura -B`      | Store a JSON record of all installed packages.                  |
| `aura -Br`     | Restore a saved record. Rolls back and uninstalls as necessary. |
| `aura -Bc <n>` | Delete all but the most recent `n` saved states.                |

### Downgrading via the Package Cache

| Command             | Function                                                            |
|---------------------|---------------------------------------------------------------------|
| `aura -C <package>` | Downgrade a package.                                                |
| `aura -Cs <regex>`  | Search the package cache for files that match a regex.              |
| `aura -Cc <n>`      | Delete all but the most recent `n` versions of each cached package. |

### Searching the Pacman Log

| Command              | Function                                         |
|----------------------|--------------------------------------------------|
| `aura -L`            | View the Pacman log.                             |
| `aura -Li <package>` | View the install / upgrade history of a package. |
| `aura -Ls <regex>`   | Search the Pacman log via a regex.               |

### Managing Orphan Packages

Orphan packages are those whose install reason is marked as "As Dependency", but
are not actually depended upon by any installed package.

| Command                     | Function                                                     |
|-----------------------------|--------------------------------------------------------------|
| `aura -O`                   | Display orphan packages.                                     |
| `aura -O --adopt <package>` | Change a package's install reason to `Explicitly installed`. |
| `aura -Oj`                  | Uninstall all orphan packages.                               |

## Localisation

As mentioned in the Philosophy above, adding new languages to Aura is quite
easy. If you speak a language other than those available and would like it added
to Aura, please consult [LOCALISATION.md](./aura/LOCALISATION.md).

## Credits

Aura has been translated by these generous people:

| Language   | Translators                                     |
|------------|-------------------------------------------------|
| Chinese    | Kai Zhang                                       |
| Croatian   | Denis Kasak and "stranac"                       |
| Dutch      | Joris Blanken                                   |
| Esperanto  | Zachary "Ghosy" Matthews                        |
| French     | Ma Jiehong and Fabien Dubosson                  |
| German     | Lukas Niederbremer and Jonas Platte             |
| Indonesian | "pak tua Greg"                                  |
| Italian    | Bob Valantin and Cristian Tentella              |
| Japanese   | Colin Woodbury and Onoue Takuro                 |
| Norwegian  | "chinatsun"                                     |
| Polish     | Chris Warrick                                   |
| Portuguese | Henry Kupty, Thiago Perrotta, and Wagner Amaral |
| Russian    | Kyrylo Silin, Alexey Kotlyarov                  |
| Serbian    | Filip Brcic                                     |
| Spanish    | Alejandro Gómez and Sergio Conde                |
| Swedish    | Fredrik Haikarainen and Daniel Beecham          |

Aura's logo is thanks to Cristiano Vitorino (@cristianovitorino).

# The `aur` Haskell Library

A library for accessing the AUR.

# The `aur-security` Tool

Performs a sweep of all PKGBUILDs on the [AUR](https://aur.archlinux.org/),
looking for Bash misuse.
