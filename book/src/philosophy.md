# The Aura Philosophy

The Aura project began in 2012 as an answer to the shortcomings of programs
often called _AUR helpers_. Certain ground rules were set in the beginning to
guide development. They are:

### Aura is Pacman

Aura doesn't just mimic `pacman`; it *is* `pacman`. All `pacman` operations and
their sub-options are accepted, as-is.

### Arch is Arch - AUR is AUR

Aura does not augment or alter `pacman`'s commands in any way.

`-S` yields repository packages and <u>only</u> those. In Aura, the `-A` operation is
introduced for obtaining AUR packages. `-A` comes with sub-options you're used
to (`-u`, `-s`, `-i`, etc.) and adds new ones to enhance AUR interaction.

### Downgradibility

Aura allows you to downgrade individual packages to previous versions with `-C`.
It also handles snapshots of your entire system, so that you can roll back whole
sets of packages when problems arise. The option `-B` will save a package state,
and `-Br` will restore a state you select. `-Au` also invokes a save
automatically.

### Independence

Aura has its own configuration file, its own local package cache, and its own
[Metadata Server][faur] called the Faur. The Faur in particular keeps traffic
off the main AUR server and allows us to provide unique package lookup schemes
not otherwise available.

### Arch Linux for Everyone

English is the dominant language of computing and the internet. That said, it's
natural that some people are going to be more comfortable working in their
native language. From the beginning, Aura has been built with multiple-language
support in mind, making it very easy to add new ones via the [Project
Fluent][fluent] format.

[faur]: https://git.sr.ht/~fosskers/faur
[fluent]: https://projectfluent.org/
