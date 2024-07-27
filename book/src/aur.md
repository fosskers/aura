# Installing AUR Packages

Let's walk through the full process of installing an AUR package. This will show
us how to discover packages, install them, and upgrade them.

> **💡 Tip:** Just want to update all your AUR packages? Aura's author uses
> `aura -Au`. Remember, no need to call `sudo` yourself.

> **💡 Tip:** For the full list of all options with detailed descriptions, see
> `man aura`.

## Installing a Package

### Searching for a Package

Let's say we want to install a package that can render `.md` README files for
us. First, we search the AUR for candidates:

```
> aura -As readme
aur/python-grip 4.6.1-1 (22 | 0.00) 
    Preview GitHub Markdown files like Readme locally before committing them
aur/python-grip-git 4.5.2-1 (15 | 0.00) 
    Preview GitHub Markdown files like Readme locally before committing them.
aur/ruby-github-markup 4.0.2-1 (4 | 0.00) 
    The code GitHub uses to render README.markup
aur/k810-conf 0.1-9 (3 | 0.00) 
    Logitech K810 Keyboard Configurator, change function keys (F-keys) behavior. Read USAGE at https://aur.archlinux.org/cgit/aur.git/tree/README.md?h=k810-conf
aur/cargo-readme 3.3.1-1 (3 | 0.24) 
    A cargo subcommand to generate README.md content from doc comments
... etc ...
```

By default, results are ordered by their vote count. If `-As` filled the screen
and we only wish to see a few results, we can filter with `--limit`:

```
> aura -As readme --limit 3
aur/python-grip 4.6.1-1 (22 | 0.00) 
    Preview GitHub Markdown files like Readme locally before committing them
aur/python-grip-git 4.5.2-1 (15 | 0.00) 
    Preview GitHub Markdown files like Readme locally before committing them.
aur/ruby-github-markup 4.0.2-1 (4 | 0.00) 
    The code GitHub uses to render README.markup
```

`--abc` can be used to sort alphabetically instead.

### Scrutinizing a Package

Alright, `python-grip` looks good. Let's take a closer look...

```
> aura -Ai python-grip
Repository    : aur
Name          : python-grip
Version       : 4.6.1-1
AUR Status    : Up to Date
Maintainer    : pancho
Project URL   : https://github.com/joeyespo/grip
AUR URL       : https://aur.archlinux.org/packages/python-grip
License       : MIT
Groups        : 
Provides      : 
Depends On    : python python-docopt python-flask python-markdown python-path-and-address python-pygments python-requests
Make Deps     : python-setuptools
Optional Deps : 
Check Deps    : 
Votes         : 22
Popularity    : 0.00
Description   : Preview GitHub Markdown files like Readme locally before committing them
Keywords      : 
Submitted     : 2017-02-09
Updated       : 2022-04-17
```

Does the PKGBUILD look alright?

```
> aura -Ap python-grip
# Maintainer: pancho horrillo <pancho at pancho dot name>
# Contributor: Clayton Craft <clayton at craftyguy dot net>

pkgname=python-grip
pkgver=4.6.1
pkgrel=1
pkgdesc="Preview GitHub Markdown files like Readme locally before committing them"
arch=('any')
license=('MIT')
url="https://github.com/joeyespo/grip"
depends=('python' 'python-docopt' 'python-flask' 'python-markdown'
         'python-path-and-address' 'python-pygments' 'python-requests')
makedepends=('python-setuptools')
source=("$pkgname-$pkgver.tar.gz::https://github.com/joeyespo/grip/archive/v$pkgver.tar.gz")
sha256sums=('6bc3883f63395c566101187bc1f1d103641c99913b7122f942d56e108e649d83')

package() {
  cd grip-$pkgver
  python setup.py install --root="$pkgdir" --optimize=1
  install -Dm644 LICENSE "$pkgdir"/usr/share/licenses/$pkgname/LICENSE
}
```

Nothing nefarious here. 

> **💡 Tip:** It's important to confirm the content of the PKGBUILD like this,
> as these are raw Bash commands that will be executed on your system during the
> build process.

### A Normal Install

```
> aura -A python-grip
aura :: Determining dependencies...
aura :: Repository dependencies:
 python-docopt
aura :: AUR packages:
 python-grip
 python-path-and-address
aura :: Proceed? [Y/n] 

... pacman output ...

aura :: Preparing build directories...
aura :: Building python-path-and-address...

... makepkg output ...

aura :: Done.
```

A few things to note:

- `python-grip` has both official and AUR dependencies. These have to be built
  and installed in a specific order for `python-grip` to even build.
- Under the hood, Aura calls `makepkg` to drive the build and `pacman` to
  finalise the installation.
- If two or more packages don't depend on each other, they'll be built one after
  another and installed at the same time. This avoids needless user prompting.

> **💡 Tip:** Aura used to have an option `-x` to expose `makepkg` output. This
> is now the default behaviour.

### Automatically Removing `makedepends`

There's a difference between the dependencies that a package needs to *build*
and the dependencies it needs to *run*. We call the former *makedepends*. Once a
package is installed, we no longer need its makedepends sitting around on our
machine. Adding `-a` to `-A` will automatically clear them out:

```
> aura -Aa grimshot
aura :: Determining dependencies...
aura :: Repository dependencies:
 scdoc
aura :: AUR packages:
 grimshot
aura :: Proceed? [Y/n] 

... the usual ...

aura :: Done.
checking dependencies...

Packages (1) scdoc-1.11.3-1

Total Removed Size:  0.03 MiB

:: Do you want to remove these packages? [Y/n] 
```

`scdoc` was only necessary during the build, so we're prompted to uninstall it.

If you'd like to turn this behaviour on permanently, you can set it within
config:

```toml
[aur]
delmakedeps = true
```

### Altering the PKGBUILD Before Building

Sometimes you want to change something specific about how a package is built.
Without a tool like Aura, you'd clone the package from the AUR, edit the
PKGBUILD, and then run `makepkg`, handling dependencies yourself.

Aura's `--hotedit` flag will let you edit a PKGBUILD on-the-fly. In the example
below, I add `echo "I CHANGED THE PKGBUILD"` to the build commands:

```
> aura -Aa python-grip --hotedit
aura :: Determining dependencies...

... the usual ...

aura :: Building python-grip...
aura :: Edit the PKGBUILD? [Y/n] 

... Your EDITOR opens, and you save your changes to the real PKGBUILD file ...

==> Making package: python-grip 4.6.1-1 (Sat 13 Jul 2024 07:08:37 AM JST)
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

If you'd like to always be prompted for hoteditting, you can set it within
config:

```toml
[aur]
hotedit = true
```

### PKGBUILD Analysis

If requested, Aura will run [Shellcheck](https://www.shellcheck.net/) on
PKGBUILDs to check for oddities:

```
> aura -A goverlay-git --shellcheck

... the usual ...

aura :: Building goverlay-git...

In PKGBUILD line 32:
  for i in "${pkgname%-git}.lpi"; do
           ^-------------------^ SC2066 (error): Since you double quoted this, it will not word split, and the loop will only run once.

For more information:
  https://www.shellcheck.net/wiki/SC2066 -- Since you double quoted this, it ...
aura :: Proceed? [Y/n]
```

If you'd like to always run `shellcheck` this way, you can set it within config:

```toml
[aur]
shellcheck = true
```

### Jailed Building via `pkgctl build`

For extra security, you can build packages in a `chroot`. This ensures that the
build process will not affect your existing filesystem. For the moment, this
option is gated by configuration; no CLI flag is available:

```toml
[aur]
chroot = ["fortls", "timelineproject-hg"]
```

This means that when these particular packages are built, it will be done in a
`chroot`. Transitive AUR dependencies will be injected properly into the build
environment.

### Using a "build user"

The flags `--build` and `--builduser` can be used to alter where the building
occurs and under which user it occurs, respectively. While usually not necessary
by default, these can be useful for system administrators who want stricter
control.

### Blindly Accepting all Prompts

Tired of pressing the `Enter` key? Or maybe you've automated `aura` into a
script. In these cases, you may want to accept all prompts automatically.
`pacman` exposes the `--noconfirm` flag for this, which also affects Aura.

## Updating your AUR Packages

`aura -Au` is the standard command, and is run without `sudo`.

```
> aura -Au
aura :: Fetching package information...
aura :: Comparing package versions...
aura :: AUR packages to upgrade:
 ghcup-hs-bin :: 0.1.22.0-1       -> 0.1.30.0-1
 tic-80-git   :: r2633.687fb340-1 -> r2883.3cf27c5e-1
 yed          :: 1:3.23.2-1       -> 1:3.24-1
aura :: Determining dependencies...
aura :: Repository dependencies:

... redacted ...

aura :: AUR packages:
 ghcup-hs-bin
 tic-80-git
 yed
aura :: Proceed? [Y/n]
```

The options that apply to normal `-A`, like `-a` and `--hotedit`, also apply
here. Of course, it's simplest to set those in config if you know your
preferences. See `man aura` or `aura -Ah` for more options.

### Displaying PKGBUILD Changes

Hey wait a minute, aren't we supposed to check PKGBUILDs before building
packages? `-k` will show us any changes that were made to a PKGBUILD compared to
the version we have installed:

```diff
> aura -Auk
aura :: Fetching package information...
aura :: Comparing package versions...
aura :: AUR packages to upgrade:
 ghcup-hs-bin :: 0.1.22.0-1 -> 0.1.30.0-1
 yed          :: 1:3.23.2-1 -> 1:3.24-1
aura :: Determining dependencies...
aura :: AUR packages:
 ghcup-hs-bin
 yed
aura :: Proceed? [Y/n] 
aura :: Preparing build directories...
aura :: Building yed...
aura :: Display diffs of build files? [Y/n] 
diff --git a/.SRCINFO b/.SRCINFO
index bc810a7..32f1f1f 100644
--- a/.SRCINFO
+++ b/.SRCINFO
@@ -1,6 +1,6 @@
 pkgbase = yed
        pkgdesc = Very powerful graph editor written in java
-       pkgver = 3.23.2
+       pkgver = 3.24
        pkgrel = 1
        epoch = 1
        url = http://www.yworks.com/en/products_yed_about.html
@@ -9,11 +9,11 @@ pkgbase = yed
        license = custom
        depends = hicolor-icon-theme
        depends = java-runtime
-       source = https://www.yworks.com/resources/yed/demo/yEd-3.23.2.zip
+       source = https://www.yworks.com/resources/yed/demo/yEd-3.24.zip
        source = yed.desktop
        source = yed
        source = graphml+xml-mime.xml
-       sha256sums = 4f96611718df696de2f33eeb2cd78bfbbdacce52390afea8d00441b6fb175e20
+       sha256sums = 842909f6e4c15399b660f316056499e63e931f95ade43d850045d852d3128947
        sha256sums = cc6957cde6eba0d82ea523b0257f8c91fd1e330a1e2ad7d64890e48a2450aa98
        sha256sums = 731b54c6e731704efe9847d78e2df474d59042452ace29d2786d76891295249e
        sha256sums = e751b69ed8a25faf46d4e4016ed8f1774abc88679067934a6081348e3d6fc332
diff --git a/PKGBUILD b/PKGBUILD
index f9ea04e..cb06126 100644
--- a/PKGBUILD
+++ b/PKGBUILD
@@ -10,7 +10,7 @@
 # https://github.com/michaellass/AUR
 
 pkgname=yed
-pkgver=3.23.2
+pkgver=3.24
 pkgrel=1
 epoch=1
 pkgdesc='Very powerful graph editor written in java'
@@ -22,7 +22,7 @@ source=("https://www.yworks.com/resources/yed/demo/yEd-${pkgver}.zip"
         'yed.desktop'
         'yed'
         'graphml+xml-mime.xml')
-sha256sums=('4f96611718df696de2f33eeb2cd78bfbbdacce52390afea8d00441b6fb175e20'
+sha256sums=('842909f6e4c15399b660f316056499e63e931f95ade43d850045d852d3128947'
             'cc6957cde6eba0d82ea523b0257f8c91fd1e330a1e2ad7d64890e48a2450aa98'
             '731b54c6e731704efe9847d78e2df474d59042452ace29d2786d76891295249e'
             'e751b69ed8a25faf46d4e4016ed8f1774abc88679067934a6081348e3d6fc332')
aura :: Proceed? [Y/n]
```

Once again, nothing evil-looking here. If you'd like to always see diffs like
this, you can set it within config:

```toml
[aur]
diff = true
```

### Including `*-git` Packages

The AUR has many packages postfixed with `-git`, `-svn`, etc. These typically
pull straight from the `master` branch of some code respository, and so
comparing version numbers to detect updates doesn't always work.

`--git` will consider all such packages for updates:

```
> aura -Au --git
aura :: Fetching package information...
aura :: Comparing package versions...
aura :: AUR packages to upgrade:
 ghcup-hs-bin :: 0.1.22.0-1 -> 0.1.30.0-1
 yed          :: 1:3.23.2-1 -> 1:3.24-1
aura :: VCS packages to rebuild:
 clasp-cl-git
 libmgba-git
 mgba-qt-git
 timelineproject-hg
aura :: Determining dependencies...
aura :: Repository dependencies:
 qt6-tools
aura :: AUR packages:
 clasp-cl-git
 ghcup-hs-bin
 mgba-git
 timelineproject-hg
 yed
aura :: Proceed? [Y/n] 
```

If you'd like to always consider such packages with `-u`, you can set it within
config:

```toml
[aur]
git = true
```
