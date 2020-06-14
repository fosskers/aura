# Managing Orphan Packages

Orphan packages are those marked as `Installed as a dependency for another
package`, but which are missing the parent package that depended on them. This
can occur when a package with many dependencies is installed and then later
removed via a single `-R` (without `-su`). Or when a package is upgraded, and
the new version no longer requires a certain dependency.

In these cases, the dependencies are left behind and becomes orphans, chewing up
disk space.

The `-O` command can help deal with these.

## What packages have become orphans?

```
> aura -O
python-docopt
python-flask
python-path-and-address
```

Weird! Are they really not needed?

```
> aura -Qi python-docopt
Name            : python-docopt
Version         : 0.6.2-7
Description     : Pythonic argument parser, that will make you smile
Architecture    : any
URL             : https://github.com/docopt/docopt
Licenses        : MIT
Groups          : None
Provides        : None
Depends On      : python
Optional Deps   : None
Required By     : None
Optional For    : None
Conflicts With  : None
Replaces        : None
Installed Size  : 83.17 KiB
Packager        : Evangelos Foutras <evangelos@foutrelis.com>
Build Date      : Thu Oct 31 09:48:34 2019
Install Date    : Fri Jun 12 09:43:12 2020
Install Reason  : Installed as a dependency for another package
Install Script  : No
Validated By    : Signature
```

Sure enough, `Required By: None`.

## Uninstalling Orphans

Clearing orphans doesn't just save us space now - it saves the space of all
future upgrades we won't have to download anymore.

`-Oj` will uninstall all such packages. Under the hood, it passes `-Rsu` to
`pacman`, hence there are more packages to uninstall than first appeared in the
`-O` list above.

```
> sudo aura -Oj
checking dependencies...

Package (5)              Old Version  Net Change

python-itsdangerous      1.1.0-4       -0.11 MiB
python-werkzeug          1.0.1-2       -2.13 MiB
python-docopt            0.6.2-7       -0.08 MiB
python-flask             1.1.2-2       -0.80 MiB
python-path-and-address  2.0.1-1       -0.01 MiB

Total Removed Size:  3.12 MiB

:: Do you want to remove these packages? [Y/n]
```

## Adopting an Orphan

Changing a package's install reason from "dependency" to "explicitly installed"
is possible via `pacman` alone, but Aura offers a shorthand:

```
> sudo aura -Oa python-path-and-address
python-path-and-address: install reason has been set to 'explicitly installed'
```
