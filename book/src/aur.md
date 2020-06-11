# Installing AUR Packages

Let's walk through the full process of installing an AUR package. This will show
us how to discover packages, how to install them, and how to deal with problems.

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

Okay, no output and no error code, so we should be safe to proceed.

### A Normal Install

### A Verbose Install

### Automatically Removing `makedepends`

### Altering the PKGBUILD Before Building

## Updating your AUR Packages

### Displaying PKGBUILD Changes

### Including `*-git` Packages

### Forcing a Rebuild
