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
aura -S firefox
```

> **💡 Note:** Unlike with `pacman`, prefixing with `sudo` is not necessary for
> "admin" actions. Aura knows when `sudo` is necessary and will prompt you.

### Update all official packages

The classic command.

```
aura -Syu
```

### Install a package built with `makepkg`

```
aura -U foobar-1.2.3-1-x86_64.pkg.tar.xz
```

## Removing Packages

### The package and all unneeded dependencies

From the manpage of `pacman`:

> This operation is recursive and analogous to a backwards `--sync` operation,
> and it helps keep a clean system without orphans.

```
aura -Rsu firefox
```

### The package and everything that depends on it

Use this with care. See `man pacman` for more details.

```
aura -Rcu firefox
```

## Querying your System

### Searching an exact package

```
aura -Qi firefox
```

### Searching a local package by description

```
> aura -Qs browser
local/firefox 127.0.2-1
    Fast, Private & Safe Web Browser
local/mathjax 3.2.2-1
    An open source JavaScript display engine for mathematics that works in all modern browsers
local/qt6-webengine 6.7.2-1 (qt6)
    Provides support for web applications using the Chromium browser project
```

### Producing a list of installed packages

```
> aura -Q
aalib 1.4rc5-18
abcl 1.9.2-1
abseil-cpp 20240116.2-2
acl 2.3.2-1
... etc ...
```

### Discovering what package owns a certain file

```
> aura -Qo firefox
/usr/bin/firefox is owned by firefox 127.0.2-1
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

### Listing all installed "foreign" packages

```
> aura -Qm
abcl 1.9.2-1
anki 2.1.66-1
babashka-bin 1.3.191-1
bashate 2.1.1-1
ccextractor 0.94-3
ccl 1.12.2-4
... etc ...
```

## Clearing your Package Cache

Pacman stores its built packages in `/var/cache/pacman/pkg/`. This directory can
grow quite large, but the following commands can help selectively clear it.

> **💡 Tip:** The Aura command `-Cc` offers additional control over clearing
> the cache, and targets Aura's cache as well.

### Removing the tarballs of uninstalled packages

```
aura -Sc
```

### Removing *all* tarballs

```
aura -Scc
```
