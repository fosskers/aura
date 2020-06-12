# Downgrading Packages

The `-C` command is used to interact with the Package Cache.

## Searching the Cache

`-Cs` can show us what's available in the cache.

```
> aura -Cs firefox
/var/cache/pacman/pkg/firefox-75.0-2-x86_64.pkg.tar.zst
/var/cache/pacman/pkg/firefox-76.0.1-1-x86_64.pkg.tar.zst
```

## Downgrading or Installing "Lost" Versions

Let's say the newest version of some package is somehow broken. Let's downgrade:

```
> sudo aura -C firefox
aura >>= What version of firefox do you want?
1. /var/cache/pacman/pkg/firefox-75.0-2-x86_64.pkg.tar.zst
2. /var/cache/pacman/pkg/firefox-76.0.1-1-x86_64.pkg.tar.zst
>> 1
loading packages...
warning: downgrading package firefox (76.0.1-1 => 75.0-2)
resolving dependencies...
looking for conflicting packages...

Package (1)  Old Version  New Version  Net Change

firefox      76.0.1-1     75.0-2        -2.08 MiB

Total Installed Size:  184.97 MiB
Net Upgrade Size:       -2.08 MiB

:: Proceed with installation? [Y/n]

... pacman output ...
```

In fact, `-C` works even if we no longer have that package installed. All that
matters is whether you have a copy of the old version in your cache.

## Cleaning the Cache

`-Cc` can help keep our cache small.

```
> sudo aura -Cc 2
aura >>= The cache contains 2050 packages, consuming 5887 megabytes.
aura >>= 2 of each package file will be kept.
aura >>= The rest will be deleted. Okay? [Y/n]
aura >>= Cleaning package cache...
aura >>= 529 megabytes freed.
```
