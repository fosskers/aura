# Installing AUR Packages

Let's walk through the full process of installing an AUR package. This will show
us how to discover packages, install them, and upgrade them.

> **💡 Tip:** Just want to update all your AUR packages? Aura's author uses
> `sudo aura -Auax`.

> **💡 Tip:** For the full list of all options with detailed descriptions, see
> `man aura`.

## Installing a Package

### Searching for a Package

Let's say we want to install a package that can render `.md` files for us.
First, we search the AUR for candidates:

```
> aura -As readme
aur/gtk3-mushrooms 3.24.11-1 (28 | 1.29)
    GTK3 patched for classic desktops like XFCE or MATE. Please see README.
aur/python-grip-git 4.5.2-1 (15 | 0.00)
    Preview GitHub Markdown files like Readme locally before committing them.
aur/python-grip 4.5.2-1 (13 | 0.14)
    Preview GitHub Markdown files like Readme locally before committing them
aur/gtk3-classic 3.24.14-1 (7 | 1.63)
    GTK3 patched for classic desktops like XFCE or MATE. Please see README.
aur/ruby-github-markup 3.0.4-1 (4 | 0.00)
    The code GitHub uses to render README.markup
... etc ...
```

By default, results are ordered by their vote count. If `-As` filled the screen
and we only wish to see a few results, we can filter with `--head`:

```
> aura -As readme --head 3
aur/gtk3-mushrooms 3.24.11-1 (28 | 1.28)
    GTK3 patched for classic desktops like XFCE or MATE. Please see README.
aur/python-grip-git 4.5.2-1 (15 | 0.00)
    Preview GitHub Markdown files like Readme locally before committing them.
aur/python-grip 4.5.2-1 (13 | 0.14)
    Preview GitHub Markdown files like Readme locally before committing them
```

`--abc` can be used to sort alphabetically instead.

### Scrutinizing a Package

Alright, `python-grip` looks good. Let's take a closer look...

```
> aura -Ai python-grip
Repository  : aur
Name        : python-grip
Version     : 4.5.2-1
AUR Status  : Up to Date
Maintainer  : craftyguy
Project URL : https://github.com/joeyespo/grip
AUR URL     : https://aur.archlinux.org/packages/python-grip
License     : MIT
Depends On  : python python-docopt python-flask python-markdown python-path-and-address python-pygments python-requests
Build Deps  : python-setuptools
Votes       : 13
Popularity  : 0.14
Description : Preview GitHub Markdown files like Readme locally before committing them
```

Does the PKGBUILD look alright?

```
> aura -Ap python-grip
# Maintainer: Clayton Craft <clayton at craftyguy dot net>

pkgname=python-grip
pkgver=4.5.2
pkgrel=1
pkgdesc="Preview GitHub Markdown files like Readme locally before committing them"
arch=('any')
license=('MIT')
url="https://github.com/joeyespo/grip"
depends=('python' 'python-docopt' 'python-flask' 'python-markdown'
         'python-path-and-address' 'python-pygments' 'python-requests')
makedepends=('python-setuptools')
source=("$pkgname-$pkgver.tar.gz::https://github.com/joeyespo/grip/archive/v$pkgver.tar.gz")
sha256sums=('bdf8949f33470e9ef9e3f09596b72cda968116ff32f0280baabe837c2ad1b29b')

package() {
  cd grip-$pkgver
  python setup.py install --root="$pkgdir" --optimize=1
  install -Dm644 LICENSE "$pkgdir"/usr/share/licenses/$pkgname/LICENSE
}
```

Looks good, but let's make sure we didn't miss anything by using Aura's PKGBUILD
security scanning:

```
> aura -Ap python-grip | aura -P
```

Okay, no output and no error code, so we should be safe to proceed. Normally you
don't need to do `-P` yourself - the same check happens automatically before
building packages with `-A`. See [PKGBUILD Security Analysis](security.md) for
more information.

### A Normal Install

```
> sudo aura -A python-grip
aura >>= Determining dependencies...

aura >>= Repository dependencies:
python-docopt
python-flask
aura >>= AUR dependencies:
python-path-and-address
aura >>= AUR Packages:
python-grip
aura >>= Continue? [Y/n]
resolving dependencies...
looking for conflicting packages...

Package (4)                    New Version  Net Change

community/python-itsdangerous  1.1.0-4        0.11 MiB
community/python-werkzeug      1.0.1-2        2.13 MiB
community/python-docopt        0.6.2-7        0.08 MiB
community/python-flask         1.1.2-2        0.80 MiB

Total Installed Size:  3.11 MiB

:: Proceed with installation? [Y/n]
... pacman output ...

aura >>= Building python-path-and-address...
loading packages...
resolving dependencies...
looking for conflicting packages...

Package (1)              New Version  Net Change

python-path-and-address  2.0.1-1        0.01 MiB

Total Installed Size:  0.01 MiB

:: Proceed with installation? [Y/n]
... pacman output ...

aura >>= Building python-grip...
loading packages...
resolving dependencies...
looking for conflicting packages...

Package (1)  New Version  Net Change

python-grip  4.5.2-1        0.34 MiB

Total Installed Size:  0.34 MiB

:: Proceed with installation? [Y/n]
... pacman output ...
```

A few things to note:

- `python-grip` has both official and AUR dependencies. These have to be built
  and installed in a specific order for `python-grip` to even build.
- Aura calls `makepkg` under the hood. By default, the output of `makepkg` is
  hidden.
- If two or more packages don't depend on each other, they'll be built one after
  another and installed at the same time. This avoids needless user prompting.

### A Verbose Install

But what if we *do* want to see the output from `makepkg`? For long builds (e.g.
[aseprite](https://aur.archlinux.org/packages/aseprite/)), it can be reassuring
to see the ongoing build output.

Let's add `-x` to `-A`:

```
> sudo aura -Ax python-grip
aura >>= Determining dependencies...

aura >>= Repository dependencies:
python-docopt
python-flask
aura >>= AUR dependencies:
python-path-and-address
aura >>= AUR Packages:
python-grip
aura >>= Continue? [Y/n]

... pacman output ...

aura >>= Building python-path-and-address...
==> Making package: python-path-and-address 2.0.1-1 (Fri 12 Jun 2020 09:39:46 AM PDT)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading v2.0.1.tar.gz...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   133  100   133    0     0     70      0  0:00:01  0:00:01 --:--:--    70
100  5130    0  5130    0     0   2197      0 --:--:--  0:00:02 --:--:--  2197
==> Validating source files with md5sums...
    v2.0.1.tar.gz ... Passed
==> Extracting sources...
  -> Extracting v2.0.1.tar.gz with bsdtar
==> Entering fakeroot environment...
==> Starting package()...

... makepkg output ...

==> Finished making: python-path-and-address 2.0.1-1 (Fri 12 Jun 2020 09:39:49 AM PDT)

... etc ...
```

Wonderful.

### Automatically Removing `makedepends`

There's a difference between the dependencies that a package needs to *build*
and the dependencies it needs to *run*. We call the former *makedepends*. Once a
package is installed, we no longer need its makedepends sitting around on our
machine. Adding `-a` to `-A` will automatically clear them out:

```
> sudo aura -Axa python-grip
aura >>= Determining dependencies...

... the usual ...

Package (1)  Old Version  New Version  Net Change

python-grip  4.5.2-1      4.5.2-1        0.00 MiB

Total Installed Size:  0.34 MiB
Net Upgrade Size:      0.00 MiB

:: Proceed with installation? [Y/n]

... pacman output ...
```

Ah, there were none in this case. Since `python-grip` is a Python package, it
never really has `makedepends`. That's okay - it's a good habit to use `-a`,
especially when updating all your AUR packages at once (see below).

### Altering the PKGBUILD Before Building

Sometimes you want to change something specific about how a package is built.
Without an AUR helper, you'd clone the package from the AUR, edit the PKGBUILD,
and then run `makepkg`, handling dependencies yourself.

Aura's `--hotedit` flag will let you edit a PKGBUILD on-the-fly. In the example
below, I added `echo "I CHANGED THE PKGBUILD"` to the build commands:

```
> sudo aura -Axa python-grip --hotedit
aura >>= Determining dependencies...

... the usual ...

aura >>= Building python-grip...
aura >>= Would you like to edit the PKGBUILD of python-grip? [Y/n]

... Your EDITOR opens, and your changes are saved to the real PKGBUILD file ...

==> Making package: python-grip 4.5.2-1 (Fri 12 Jun 2020 09:52:26 AM PDT)
==> Checking runtime dependencies...
==> Checking buildtime dependencies...
==> Retrieving sources...
  -> Downloading python-grip-4.5.2.tar.gz...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   121  100   121    0     0    288      0 --:--:-- --:--:-- --:--:--   289
100  174k    0  174k    0     0   165k      0 --:--:--  0:00:01 --:--:--  938k
==> Validating source files with sha256sums...
    python-grip-4.5.2.tar.gz ... Passed
==> Extracting sources...
  -> Extracting python-grip-4.5.2.tar.gz with bsdtar
==> Entering fakeroot environment...
==> Starting package()...
I CHANGED THE PKGBUILD

... normal makepkg output, etc ...
```

Some things to note:

- If `.install` or `.patch` files are present, `--hotedit` will also prompt you
  to edit those.
- You can change the dependency lists, but **this will only affect `makepkg`'s
  dependency resolution, not Aura's**. If you want Aura to skip dependency
  checks completely because you know you're right (and you know `makepkg` will
  succeed after you hotedit), use the `--skipdepcheck` flag. See issue
  [#605](https://github.com/fosskers/aura/issues/605) for more information.
- If you want customizations to a PKGBUILD to apply to all future builds of a
  package, it's recommended that you maintain a separate PKGBUILD and call
  `makepkg` yourself.

## Updating your AUR Packages

`sudo aura -Ayu` works, but `sudo aura -Auax` is a more common way to update
your installed AUR packages.

- `-A`: Only consider AUR packages.
- `-u`: Update all packages that can be.
- `-a`: Uninstall unneeded makedepends afterward.
- `-x`: Display `makepkg` output as we go.

```
> sudo aura -Auax
aura >>= Fetching package information...
aura >>= Comparing package versions...
aura >>= AUR Packages to upgrade:
aseprite :: 1.2.16.2-3 -> 1.2.19.2-1

aura >>= Saved package state.
aura >>= Determining dependencies...

aura >>= Repository dependencies:
ninja
aura >>= AUR Packages:
aseprite
aura >>= Continue? [Y/n]
```

Note that `-u` automatically invoked a saving of the current package state. If
you update your system and there is a serious problem, you can roll back with
`-Br` and return to the state that was just saved. See [Package Set
Snapshots](snapshots.md) for more detail.

### Displaying PKGBUILD Changes

Hey wait a minute, aren't we supposed to check PKGBUILDs before building
packages? `-k` will show us any changes that were made to a PKGBUILD compared to
the version we have installed:

```diff
> sudo aura -Auaxk
aura >>= Fetching package information...
aura >>= Comparing package versions...
aura >>= AUR Packages to upgrade:
aseprite :: 1.2.16.2-3 -> 1.2.19.2-1

aura >>= Saved package state.
aura >>= aseprite PKGBUILD changes:
--- /var/cache/aura/pkgbuilds/aseprite.pb	2020-02-26 14:11:52.427913916 -0800
+++ /tmp/new.pb	2020-06-12 10:19:55.564270161 -0700
@@ -7,50 +7,57 @@
 # Contributor: Kamil Biduś <kamil.bidus@gmail.com>

 pkgname=aseprite
-pkgver=1.2.16.2
-pkgrel=3
+pkgver=1.2.19.2
+pkgrel=1
 pkgdesc='Create animated sprites and pixel art'
-arch=('x86_64' 'i686')
+arch=('x86_64')
 url="http://www.aseprite.org/"
 license=('custom')
-depends=('cmark' 'pixman' 'curl' 'giflib' 'zlib' 'libpng' 'libjpeg-turbo' 'tinyxml' 'freetype2'
-         'harfbuzz' 'nettle' 'fontconfig' 'libxcursor' 'desktop-file-utils' 'hicolor-icon-theme')
-makedepends=('cmake' 'ninja' 'git' 'python2')
+depends=('cmark' 'curl' 'giflib' 'zlib' 'libpng' 'tinyxml' 'freetype2' 'fontconfig' 'libxcursor'
+         'hicolor-icon-theme')
+makedepends=('cmake' 'ninja' 'git' 'python2' 'freeglut' 'xorgproto' 'libxi' 'harfbuzz-icu'
+             'nettle')

... etc ...

aura >>= Determining dependencies...

aura >>= Repository dependencies:
ninja
aura >>= AUR Packages:
aseprite
aura >>= Continue? [Y/n]
```

### Including `*-git` Packages

The AUR has many packages postfixed with `-git`, `-svn`, etc. These typically
pull straight from the `master` branch of some code respository, and so the idea
of comparing version numbers to detect updates doesn't work as well.

`--devel` will consider all such packages for updates:

```
> sudo aura -Auax --devel
aura >>= Fetching package information...
aura >>= Comparing package versions...
aura >>= AUR Packages to upgrade:
aseprite    :: 1.2.16.2-3 -> 1.2.19.2-1
libumem-git

aura >>= Saved package state.
aura >>= Determining dependencies...

aura >>= Repository dependencies:
ninja
aura >>= AUR Packages:
aseprite
libumem-git
aura >>= Continue? [Y/n]
```

Notice that no `a -> b` version number update was shown for the `-git` package.

> **💡 Note:** Usually packages are built in semi-randomly named directories
> within `/tmp`, which is cleared upon every reboot of the machine. (Pass
> `-c`/`--clean` to `-A` to be proactive about clearing these directories.)
>
> "VCS" packages on the other hand are stored elsewhere and given fixed names,
> so that updates invoke a `git pull` instead of a full `git clone` (which may
> be expensive!).
>
> See [the next page](storage.md) for more information.

### Forcing a Rebuild

By default, Aura won't rebuild a package if the most recent version is already
installed (or even just available in your package cache). But sometimes you *do*
want to force a rebuild. For instance, some `-git` packages have no concept of a
version number. Even with `--devel`, an update will never be detected. In these
cases, `--force` will cause the package to be completely rebuilt.

### Blindly Accepting all Prompts

Tired of pressing the `Enter` key? Or maybe you've automated `aura` into a
script. In these cases, you may want to accept all prompts automatically.
`pacman` exposes the `--noconfirm` flag for this, which also affects Aura.

> **💡 Note:** If a PKGBUILD vulnerability was detected, Aura **will exit** and
> not proceed with building, even if `--noconfirm` was given.
>
> See [PKGBUILD Security Analysis](security.md) for more information.
