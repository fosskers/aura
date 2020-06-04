# The Aura Philosophy

The Aura project began in 2012 as an answer to the shortcomings of other AUR
helpers. Certain ground-rules were set in the beginning to guide development.
They are:

### Aura is Pacman

Aura doesn't just mimic `pacman`; it *is* `pacman`. All `pacman` operations and
their sub-options are allowed. Some even hold special meaning in Aura as well.

### Arch is Arch - AUR is AUR

`-S` yields pacman packages and *only* pacman packages. In Aura, the `-A`
operation is introduced for obtaining AUR packages. `-A` comes with sub-options
you're used to (`-u`, `-s`, `-i`, etc.) and adds new ones to enhance AUR
interaction.

### Secure Package Building

PKGBUILDs from the AUR are raw `bash` code and can contain anything. It's a
user's responsibility to verify the contents of a PKGBUILD before building, but
people can make mistakes and overlook details. Aura scans PKGBUILDs before
building to detect bash misuse and other exploits. The `-P` command is also
provided for scanning your own PKGBUILDs.

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
