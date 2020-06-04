# Usage

> **💡 Did you know?** Aura has a manpage with every flag explained in detail.
> Check it out in your terminal with `man aura`.

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
```
> sudo aura -A zoom
```
- [`-B`](snapshots.md): Create and restore snapshots of installed packages.
```
> sudo aura -B
aura >>= Saved package state.
```
- [`-C`](downgrading.md): Downgrade installed packages.
```
> sudo aura -C aura-bin
aura >>= What version of aura-bin do you want?
1. /var/cache/pacman/pkg/aura-bin-2.3.0-1-x86_64.pkg.tar.xz
2. /var/cache/pacman/pkg/aura-bin-3.0.0-1-x86_64.pkg.tar.xz
3. /var/cache/pacman/pkg/aura-bin-3.0.0-2-x86_64.pkg.tar.xz
4. /var/cache/pacman/pkg/aura-bin-3.1.1-1-x86_64.pkg.tar.xz
>>
```
- [`-L`](log.md): Search and inspect the Pacman log.
```
> aura -Li firefox
Package        : firefox
First Install  : 2016-05-03 08:46
Upgrades       : 75
Recent Actions :
[2020-04-08T14:26:09-0700] [ALPM] upgraded firefox (74.0-2 -> 75.0-1)
[2020-05-04T09:20:53-0700] [ALPM] upgraded firefox (75.0-1 -> 75.0-2)
[2020-05-18T08:39:43-0700] [ALPM] upgraded firefox (75.0-2 -> 76.0.1-1)
```
- [`-O`](orphans.md): Handle "orphans" - dependencies whose parent package is gone.
```
> aura -O
ruby-bundler
vim
```
- [`-P`](security.md): Perform security analysis of PKGBUILDs.
```
> aura -Ap myget | aura -P

    sudo pacman -S aurvote

aura >>= sudo indicates that someone may be trying to gain root access to your machine.
aura >>= Potential PKGBUILD vulnerabilities detected.
```
