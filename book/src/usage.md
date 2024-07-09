# Usage

> **💡 Did you know?** Aura has a manpage with every flag explained in detail.
> Check it out in your terminal with `man aura`.

> **❗ Attention:** As of the 4.x series, `sudo` is no longer necessary when
> running Aura. When escalated privileges are required, Aura will automatically
> prompt you.

## Pacman Commands

First and foremost, Aura is compatible with `pacman`. This gives us access to
the following **Commands**:

- `-S`: Search and install official packages.
```
> aura -S firefox
```

- `-Q`: Query the database of installed packages.
```
> aura -Qi firefox
Name            : firefox
Version         : 127.0.2-1
Description     : Fast, Private & Safe Web Browser
... etc ...
```

- `-R`: Remove installed packages.
```
> aura -R firefox
```

- `-U`: Install a manually built package.
```
> makepkg
... building ...
> aura -U aura-4.0.0-1-x86_64.pkg.tar.xz
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

See [the next page](pacman.md) for a list of common Pacman idioms.

## Aura Commands

Aura also provides a number of new Commands:

- [`-A`](aur.md): Search and install packages from the AUR.
```
> aura -A qlot
```

- [`-B`](snapshots.md): Create and restore snapshots of installed packages.
```
> aura -B
aura :: Saved package state.
```

- [`-C`](downgrading.md): Downgrade installed packages.
```
> aura -C qlot
aura :: What version of qlot do you want?
 0) 1.5.6-1
 1) 1.5.1-1
>>
```

- [`-L`](log.md): Search and inspect the ALPM log.
```
> aura -Li firefox
Name           : firefox
First Install  : 2016-05-03 08:46
Upgrades       : 176
Recent Actions : 
[2024-02-24T07:29:46+0900] [ALPM] upgraded firefox (122.0.1-1 -> 123.0-1)
[2024-03-11T16:42:37+0900] [ALPM] upgraded firefox (123.0-1 -> 123.0.1-1)
[2024-03-24T15:03:33+0900] [ALPM] upgraded firefox (123.0.1-1 -> 124.0.1-1)
```

- [`-O`](orphans.md): Handle "orphans" - dependencies whose parent package is no
  longer installed.
```
> aura -O
asar 3.2.8-1
```

- `check`: Validate your system.
```
> aura check
aura :: Validating your system.
aura :: Environment
  [✓] locale -a contains LANG value? (en_US.UTF-8)
  [✓] Aura is localised to your LANG?
  [✓] EDITOR variable set?
  [✓] EDITOR value (emacs) is executable?
  [✓] Java environment set?
... etc. ...
```

- `conf`: Inspect or generate Aura configuration.
```
> aura conf --gen > ~/.config/aura/config.toml
```

- `deps`: View the dependency graph of given packages.
```
> aura deps gcc --reverse --limit=3 | dot -Tpng > deps.png
```
<p align="center">
  <img src="gcc-deps.png">
</p>

- `open`: Open various webpages related to Aura.
```
> aura open --docs
```

- `stats`: View statistics about your machine and Aura itself.
```
> aura stats
Host                 : yumi
User                 : colin
Distribution         : Arch Linux
Editor               : emacs
Installed packages   : 1144
Pacman Package Cache : 7.05GiB
Aura Package Cache   : 1.29GiB
Aura Build Cache     : 6.49GiB
/tmp Directory       : 11.31MiB
```

- `thanks`: The people behind Aura.
- `free`: List packages with potentially non-free software Licenses.
```
> aura free
adobe-source-code-pro-fonts: custom
aspell-en: custom
blas: custom
boost: custom
boost-libs: custom
cantarell-fonts: custom:SIL
... etc. ...
```


