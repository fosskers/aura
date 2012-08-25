What is Aura?
=============
Aura is a package manager for Arch Linux. It's main purpose is as an
"AUR helper", in that it automates the process of installating packages
from the Arch User Repositories. It is, however, capable of much more.

The Aura Philosophy
===================
### Aura is Pacman
  Aura doesn't just mimic pacman... it _is_ pacman.
  All pacman operations and their sub-options are allowed.
  Some even hold special meaning in Aura as well.

### Arch is Arch. AUR is AUR.
  `-S` yields pacman packages and _only_ pacman packages. This agrees with
  the above. Thus in aura, the `-A` operation is introduced for obtaining
  AUR packages. `-A` comes with with all the sub-options (`-s`, `-u`, etc.)
  that you're used to.  

### All together
Dependencies and packages are not built and installed one at a time.
Install order is as follows:

1. All pacman (ABS) dependencies (all at once).
2. All AUR dependencies (one at a time).
3. All AUR packages (all at once).

### Quiet Building
  By default `makepkg` output is suppressed. If you want the people
  behind you to think you're a badass hacker, then this suppression
  can be disabled by using `-x` alongside `-A`.

### Know your System
  Editing PKGBUILDs mid-build is not default behaviour.
  An Arch user should know _exactly_ what they're putting into their system,
  thus research into prospective packages should be done beforehand.
  However, for functionality's sake, the option `--hotedit` used with `-A`
  will prompt the user for PKGBUILD editing. Regardless, as a responsible
  user you must KNOW. WHAT. YOU. ARE. BUILDING.
  
### Downgradibility
  AUR package files that are built are moved to the package cache.
  This allows for them to be easily downgraded when problems arise.
  Other top AUR helper programs do not do this. 

### No Orphans
  Sometimes dependencies lose their *required* status, but remain
  installed on your system. Sometimes AUR package "build" dependencies
  aren't required at all after install. Packages like this just
  sit there, receiving upgrades for no reason.
  Aura helps keep track of and remove packages like this. 

### Arch Linux for Everyone
  English is well established as the world's Lingua Franca, and is also
  the dominant language of computing and the internet. That said, it's
  natural that some people are going to be more comfortable working
  in their native language. From the beginning Aura has been built with
  multiple-language support in mind, making it very easy to add new ones.

### Haskell
  Aura is written in Haskell, which means easy developing and pretty code.
  Aura code isn't complicated, and for the burgeoning Haskeller there
  are examples of things like regexes and CLI argument handling which
  could come in handy as a reference.

Sample Usage
============
#### Installing Packages
Install an AUR package:

    aura -A (package)

Upgrade all installed AUR packages:

    aura -Ayu

Checkout an AUR package's PKGBUILD:

    aura -Ap (package)

Install with makepkg output unsuppressed:

    aura -Ax (package)

Install and remove make dependencies afterwards:

    aura -Aa (package)

Go totally nuts (not recommended):

    aura -Ayuax --noconfirm --japanese

#### Working with the Package Cache
Downgrade a package (this is interactive):

    aura -C (package)

Search the package cache for package files via a regex:

    aura -Cs (regex)

Backup the package cache:

    aura -Cb (/path/to/backup/location/)

Reduce the package cache to contain only 'x' of each package file:

    aura -Cc 5

Localisation
============
As mentioned in the Philosophy above, adding new languages to Aura is
quite easy. However the currently available languages are limited to
those known by the author. If you speak a language other than those
available and would like it added to Aura, please consult the 
Localisation Guide provided.