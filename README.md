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
        - [Secure Package Building](#secure-package-building)
        - [Downgradibility](#downgradibility)
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
        - [PKGBUILD Security Analysis](#pkgbuild-security-analysis)
    - [Configuration](#configuration)
    - [Localisation](#localisation)
    - [Credits](#credits)
- [The `aur` Haskell Library](#the-aur-haskell-library)
- [The `aursec` Tool](#the-aursec-tool)

<!-- markdown-toc end -->

# Aura

## What is Aura?

Aura is a package manager for Arch Linux. Its original purpose is as an *AUR
helper*, in that it automates the process of installating packages from the Arch
User Repositories. It is, however, capable of much more.

## The Aura Philosophy

### Aura is Pacman

Aura doesn't just mimic `pacman`; it *is* `pacman`. All `pacman` operations and
their sub-options are allowed. Some even hold special meaning in Aura as well.

### Arch is Arch - AUR is AUR

`-S` yields pacman packages and *only* pacman packages. This agrees with the
above. In Aura, the `-A` operation is introduced for obtaining AUR packages.
`-A` comes with sub-options you're used to (`-u`, `-s`, `-i`, etc.).

### Secure Package Building

PKGBUILDs from the AUR can contain anything. It's a user's responsibility to
verify the contents of a PKGBUILD before building, but people can make mistakes
and overlook details. Aura scans PKGBUILDs before building to detect bash misuse
and other exploits. The `-P` command is also provided for scanning your own
PKGBUILDs.

Also, while pre-build PKGBUILD editing is not default behaviour, this can be
achieved with `--hotedit`.

### Downgradibility

Aura allows you to downgrade individual packages to previous versions with `-C`.
It also handles snapshots of your entire system, so that you can roll back whole
sets of packages when problems arise. The option `-B` will save a package state,
and `-Br` will restore a state you select. `-Su` and `-Au` also invoke a save
automatically.

### Arch Linux for Everyone

English is the dominant language of computing and the internet. That said, it's
natural that some people are going to be more comfortable working in their
native language. From the beginning, Aura has been built with multiple-language
support in mind, making it very easy to add new ones.

### Haskell

Aura is written in Haskell, which means easy development and beautiful code.
Please feel free to use it as a Haskell reference. Aura code demonstrates:

- Parser combinators (`megaparsec`)
- CLI flag handling (`optparse-applicative`)
- Concurrency (`scheduler`)
- Shell interaction (`typed-process`)
- Pretty printing (`prettyprinter`)
- Logging (`rio`)
- Modern Haskell project architecture (config, CI, distribution)

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

Full usage information can be found in Aura's man page.

### Installing Packages

| Command              | Function                                                                              |
|----------------------|---------------------------------------------------------------------------------------|
| `aura -A <package>`  | Install an AUR package.                                                               |
| `aura -Au`           | Upgrade all installed AUR packages.                                                   |
| `aura -Akuax`        | Author's favourite (upgrades, removes makedeps, shows PKGBUILD diffs, shows progress) |
| `aura -Ai <package>` | Look up information on an AUR package.                                                |
| `aura -As <regex>`   | Search the AUR via a regex.                                                           |
| `aura -Ap <package>` | Display a package's PKGBUILD.                                                         |
| `aura -Ad <package>` | List a package's dependencies.

### Package Set Snapshots

| Command        | Function                                                        |
|----------------|-----------------------------------------------------------------|
| `aura -B`      | Store a JSON record of all installed packages.                  |
| `aura -Br`     | Restore a saved record. Rolls back and uninstalls as necessary. |
| `aura -Bc <n>` | Delete all but the most recent `n` saved states.                |
| `aura -Bl`     | Show all saved package state filenames.                         |

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

| Command              | Function                                                     |
|----------------------|--------------------------------------------------------------|
| `aura -O`            | Display orphan packages.                                     |
| `aura -Oa <package>` | Change a package's install reason to `Explicitly installed`. |
| `aura -Oj`           | Uninstall all orphan packages.                               |

### PKGBUILD Security Analysis

As mentioned above, the `-P` commands can help us detect bash usage that
conflicts with the AUR guidelines, as well as outright exploits.

| Command           | Function                                        |
|-------------------|-------------------------------------------------|
| `aura -P <stdin>` | Analyse a PKGBUILD piped from `-Ap`.            |
| `aura -Pf <file>` | Analyse a PKGBUILD file.                        |
| `aura -Pd <dir>`  | Analyse the PKGBUILD file found in a directory. |
| `aura -Pa`        | Analyse all locally installed AUR packages.     |

## Configuration

Aura looks for a configuration file at `/etc/aura.conf`, but won't break if one
isn't present. A template config file [can be found here](aura/doc/aura.conf)
and contains all instructions. If you install Aura via its AUR package, this
file is added for you.

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

Aura's logo is thanks to [Cristiano Vitorino](https://github.com/cristianovitorino).

# The `aur` Haskell Library

A library for accessing the AUR.

# The `aursec` Tool

Performs a sweep of all PKGBUILDs on the [AUR](https://aur.archlinux.org/),
looking for Bash misuse.
