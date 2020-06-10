# Aura-as-Pacman

`aura` can be used in place of `pacman` in all situations. At the very least,
this is two fewer letters to type!

Below are some common `pacman` idioms for managing your system. The list is a
handy reference but not exhaustive. For everything that `pacman` is capable of,
see `man pacman`.

To find out how to use Aura to interact with the AUR, continue to [the next
page](aur.md).

## Package Installation and Updates

### Install an official package

```
sudo aura -S firefox
```

> **💡 Note:** Like with `pacman`, `sudo` must be used for all "admin" actions.

### Update all official packages

The classic command.

```
sudo aura -Syu
```

### Install a package built with `makepkg`

```
sudo aura -U foobar-1.2.3-1-x86_64.pkg.tar.xz
```

## Removing Packages

### The package and all unneeded dependencies

From the manpage of `pacman`:

> This operation is recursive and analogous to a backwards `--sync` operation,
> and it helps keep a clean system without orphans.

```
sudo aura -Rsu firefox
```

### The package and everything that depends on it

Use this with care. See `man pacman` for more details.

```
sudo aura -Rcu firefox
```

## Querying your System

### Searching an exact package

```
aura -Qi firefox
```

### Searching a local package by description

```
> aura -Qs browser
local/chromium 83.0.4103.61-1
    A web browser built for speed, simplicity, and security
local/firefox 76.0.1-1
    Standalone web browser from mozilla.org
local/qt5-webengine 5.14.2-3 (qt qt5)
    Provides support for web applications using the Chromium browser project
```

### Producing a list of installed packages

```
> aura -Qm
alsi 0.4.8-1
aseprite 1.2.16.2-3
ccextractor 0.88-2
ctop 0.7.3-1
deb2targz 0.1-7
... etc ...
```

### Discovering what package owns a certain file

```
> aura -Qo /usr/bin/firefox
/usr/bin/firefox is owned by firefox 76.0.1-1
```

### Discovering which files are brought in by a package

```
> aura -Ql firefox
firefox /usr/
firefox /usr/bin/
firefox /usr/bin/firefox
firefox /usr/lib/
firefox /usr/lib/firefox/
firefox /usr/lib/firefox/Throbber-small.gif
firefox /usr/lib/firefox/application.ini
... etc ...
```

You can use `grep` to filter what you're looking for:

```
> aura -Ql firefox | grep bin
firefox /usr/bin/
firefox /usr/bin/firefox
firefox /usr/lib/firefox/firefox-bin
```

## Clearing your Package Cache

Pacman and Aura store built packages in `/var/cache/pacman/pkg/`. This directory
can grow quite large, but the following commands can help selectively clear it.

> **💡 Note:** The Aura command `-Cc` offers additional control over clearing
> the cache.

### Removing the tarballs of uninstalled packages

```
sudo aura -Sc
```

### Removing *all* tarballs

```
sudo aura -Scc
```
