# Package Set Snapshots

Arch Linux (and Linux in general) used to have a reputation of breaking every
time packages were upgraded. Around 2010, Aura's author used to run Arch on a
Macbook, and such catastrophic breakage *did* occur every six months or so. But
fast-forward to the present day, and such problems are rare. Even so, the idea
of "whole system rollback" is spreading, and even since those early Arch-on-Mac
days, Aura has had the `-B` Command for saving and restoring entire package
sets.

## Saving a Package Set

```
> aura -B
aura :: Saved package state.
```

This saves a file like `2024.07(Jul).09.21.07.02.json` to
`~/.cache/aura/snapshots/`. If we take a peek inside, we see:

```
{
  "time": [2024, 196, 3, 54, 43, 311735304, 0, 0, 0],
  "pinned": false,
  "packages": {
    "libmm-glib": "1.22.0-1",
    "fcitx5-mozc": "2.26.4632.102.g4d2e3bd-2",
    "graphviz": "11.0.0-1",
    "mailcap": "2.1.54-1",
    "libsm": "1.2.4-1",
    "containerd": "1.7.18-1",
... etc ...
```

Simple enough - a list of all installed packages with their versions. We'll talk
about `pinned` below.

These files are in JSON format in case other tools wish to read them.

## Restoring a Package Set

Let's say I removed some packages a few days ago but am now having issues, and I
want to rollback to the system state I had at the time. Let's see that happen:

```
> aura -Br
aura :: Select a snapshot to restore:
 0) 2024-07-09 21-07-02 
 1) 2024-07-12 22-56-43 
 2) 2024-07-12 22-59-18 
 3) 2024-07-12 23-04-25 
 4) 2024-07-12 23-19-29 [pinned]
 5) 2024-07-14 03-54-37 
 6) 2024-07-14 03-54-43 
>>> 0
loading packages...
resolving dependencies...
looking for conflicting packages...

Packages (21) asar-3.2.8-1  cdparanoia-10.2-9  grimshot-1.9-1  gst-plugins-base-1.24.5-2
              kirigami2-5.116.0-1  meson-1.4.1-1  mgba-qt-git-0.11.0.r8251.6853080b9-1
              mgba-sdl-0.10.3-2  musl-1.2.5-1  neofetch-7.1.0-2  ninja-1.12.1-1  pkgstats-3.2.18-1
              python-tqdm-4.66.4-1  qt5-graphicaleffects-5.15.14-1  qt5-multimedia-5.15.14+kde+r2-1
              qt5-quickcontrols-5.15.14-1  qt5-quickcontrols2-5.15.14+kde+r5-1  typst-1:0.11.1-1
              webui-2.5.0+beta.1+14+gb65608df-1  xf86-input-libinput-1.4.0-1  xorg-server-21.1.13-1

Total Installed Size:  86.50 MiB

:: Proceed with installation? [Y/n]
```

This reinstalls the old versions of those packages and removes anything
currently installed that wasn't at the time of the snapshot.

## Clearing out old Saved States

Especially if you have automatic state-saving with `-Au` turned on, these
snapshot files can build up quickly. You might not even have most of the
packages necessary to roll back to those early states if you've recently used
`-Cc`. Let's clear out the old, unusable ones:

```
> aura -Bc 
aura :: Remove stale snapshots? [Y/n] 
aura :: Done.
```

"Stale" refers to snapshots for which the corresponding tarball of some package
no longer exists on your system. You can confirm the overall state of your
snapshots with `aura check`.

If you have edited a snapshot file to change the `pinned` field to true, then
`-Bc` will never remove it.
