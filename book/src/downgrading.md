# Downgrading Packages

The `-C` command is used to interact with the Package Cache.

## Searching the Cache

`-Cs` shows us what's available in the cache.

```
> aura -Cs firefox
/var/cache/pacman/pkg/firefox-127.0.2-1-x86_64.pkg.tar.zst
/var/cache/pacman/pkg/firefox-126.0.1-1-x86_64.pkg.tar.zst
```

We can get more intelligent output via `-Ci`:

```
> aura -Ci firefox
Name               : firefox
Latest             : 127.0.2-1 [installed]
Created            : 2024-06-29 04-12-38
Signature          : Yes
Tarball Size       : 69.01MiB
Available Versions : 127.0.2-1, 126.0.1-1
```

## Downgrading

Let's say the newest version of some package is somehow broken. Let's downgrade:

```
> aura -C firefox
aura :: What version of firefox do you want?
 0) 127.0.2-1
 1) 126.0.1-1
>>> 1
loading packages...
warning: downgrading package firefox (127.0.2-1 => 126.0.1-1)
resolving dependencies...
looking for conflicting packages...

Packages (1) firefox-126.0.1-1

Total Installed Size:  239.53 MiB
Net Upgrade Size:       -1.34 MiB

:: Proceed with installation? [Y/n]
```

In fact, `-C` works even if we no longer have that package installed. All that
matters is whether you have a copy of the old version in your cache.

## Cleaning the Cache

`-Cc` can help keep our cache small.

```
> aura -Cc 2
aura :: Current cache size: 8.31GiB
aura :: 2 of each package file will be kept. The rest will be deleted.
aura :: Proceed? [Y/n] 
aura :: 34.45MiB freed.
```
