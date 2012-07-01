WHAT IS AURA?
=============
Aura is a package manager for Arch Linux. It's main purpose is as an
`AUR helper`, in that it automates the process of installating packages
from the Arch User Repositories.

THE AURA PHILOSOPHY
===================
Aura is Pacman.
-----------------
  Aura doesn't just mimic pacman... it _is_ pacman.
  All pacman operations and their sub-options are allowed.
  Some even hold special meaning in Aura as well.

Arch is Arch. AUR is AUR.
-------------------------
  `-S` yields pacman packages and only pacman packages. This agrees with
  the above. Thus in aura, the `-A` operation is introduced for obtaining
  AUR packages. `-A` comes with with all the sub-options (-s, -u, etc.)
  that you're used to.  

All together
------------
  Dependencies and packages are not built and installed one at a time.

  Install order is as follows:

    1. All pacman (ABS) dependencies (all at once).

    2. All AUR dependencies (one at a time).

    3. All AUR packages (all at once).
  
Downgradibility
---------------
  AUR package files that are built are moved to the package cache.
  This allows for them to be easily downgraded when problems arise.
  Other top AUR helper programs do not do this. 

No Orphans
----------
  Sometimes dependencies lose their *required* status, but remain
  installed on your system. Sometimes AUR package build dependencies
  aren't required at all after install. Packages like this just
  sit there, receiving upgrades for no reason.
  Aura helps keep track of and remove packages like this. 

Arch Linux for Everyone
-----------------------
  English is well established as the world's Lingua Franca, also being
  the dominant language of computing and the internet. That said, it's
  natural that some people are going to be more comfortable working
  in their native language. From the beginning Aura has been built with
  multiple-language support in mind, making it very easy to add new ones.

Haskell
-------
  Aura is written in Haskell, which means easy developing and pretty code.
  Aura code isn't complicated, and for the burgeoning Haskeller there
  are examples of things like regexes and CLI argument handling which
  could come in handy as a reference.

LOCALISATION
============
As mentioned in the Philosophy above, adding new languages to Aura is
quite easy. However the currently available languages are limited to
those known by the author. If you speak a language other than those
available and would like it added to Aura, please consult the 
Localisation Guide provided.