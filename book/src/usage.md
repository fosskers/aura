# Usage

> **💡 Did you know?** Aura has a manpage with every flag explained in detail.
> Check it out with `man aura`.

## Pacman Commands

First and foremost, Aura is compatible with `pacman`. This gives us access to
the following **Commands**:

- `-S`: Search and install official packages.
```
> sudo aura -S firefox
```
- `-Q`: Query the database of installed packages.
```
> aura -Qi firefox
Name            : firefox
Version         : 76.0.1-1
Description     : Standalone web browser from mozilla.org
... etc ...
```
- `-R`: Remove installed packages.
```
> sudo aura -R firefox
```
- `-U`: Install a manually built package.
```
> makepkg
... building ...
> sudo aura -U aura-bin-3.1.1-1-x86_64.pkg.tar.xz
```
- `-D`: Interact with Pacman's database directly.
```
> aura -Dk
No database errors have been found!
```
- `-F`: Make queries regarding files owned by packages.
```
> aura -Fl firefox
firefox usr/
firefox usr/bin/
firefox usr/bin/firefox
firefox usr/lib/
firefox usr/lib/firefox/
firefox usr/lib/firefox/Throbber-small.gif
firefox usr/lib/firefox/application.ini
... etc ...
```
- `-T`: Check if a dependency is satisfied.
```
> aura -T firefox "qt>100"
qt>100
```
- `-V`: Display a fun version message.

See [the next page](pacman.md) for a list of common Pacman idioms.

## Aura Commands

Aura also provides a number of new Commands:

- [`-A`](aur.md): Search and install packages from the AUR.
- [`-B`](snapshots.md): Create and restore snapshots of installed packages.
- [`-C`](downgrading.md): Downgrade installed packages.
- [`-L`](log.md): Search and inspect the Pacman log.
- [`-O`](orphans.md): Handle "orphans" - dependencies whose parent package is gone.
- [`-P`](security.md): Perform security analysis of PKGBUILDs.
