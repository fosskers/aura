=============
Aura 2 Design
=============

Preface
=======

This is a design document for version 2 of `Aura`_. Note that
specifications are written in present tense, as in, “Aura does this”
even if at the time of writing those features aren’t implemented yet.
This is to ensure that the document can act as a reference for Aura’s
behaviour post-release.

Mission Statement
=================

Aura is a cross-distribution package manager for GNU/Linux systems. It
is based around a distribution-specific Hook system for custom
build/install behaviour, while maintaining a custom interface across all
distros. Aura itself provides:

-  Dependency management.

-  Package downloading.

-  Package-state backups/restoration.

Aura’s authors recognize that `attemping to create universal standards
can be problematic`_, but that is precisely why Aura exists. By having a
unified interface over multiple packaging standards, users can
transition between distributions more easily, and distribution
developers can avoid reinventing the wheel by writing their own package
management software.

Functionality
=============

General
-------

By default, Aura handles three types of packages:

**Repository Packages**
   Prebuilt binaries available direct from the user’s Distribution.

**Foreign Packages**
   Packages that generally need to be compiled by the user. Their
   versioning/source locations may be managed by the Distribution is some way.

**Local Packages**
   Packages installed on the user’s system. Records of them and the files
   belonging to them are stored in a database, and package files themselves are
   stored in a cache (in :file:`/var/cache/` or elsewhere).

A number of operations can be performed on these package types, as explained
below.

Program Flow
~~~~~~~~~~~~
.. image:: programFlow.*

Common Behaviour
~~~~~~~~~~~~~~~~
As can be gleamed from the program flow chart, the "capital letter"
operators pertaining to packages share the same functionality metaphors.

- Installation: ``aura -{S,F,L} <packages>``
- Searching: ``aura -{S,F,L}s <regex-like-pattern>``

Output sample::

   $> aura -Ss nvidia
   extra/nvidia 337.25-3 [installed]
       NVIDIA drivers for linux
   extra/nvidia-304xx 304.121-5
       NVIDIA drivers for linux, 304xx legacy branch
   extra/nvidia-304xx-libgl 304.121-2
       NVIDIA drivers libraries symlinks, 304xx legacy branch

Aura will fail silently when no pattern is given.

- Info Lookups: ``aura -{S,F,L}i <packages>``

To the question, "What does it mean to install a local package?" consider
the following the use cases::

   $> aura -L foo-1.2-1.pkg.tar.gz  -- Installing a prebuilt package tarball.

   $> aura -L bar
   aura >=> Which version of `bar` do you want?
   1. bar-1.1.1-1
   2. bar-1.2.1-1
   3. bar-1.2.3-1
   >>  -- You choose which to install from your _local_ cache.

Local Package Removal
~~~~~~~~~~~~~~~~~~~~~

Local packages may be removed singularly, or in groups.

Usage:

- Just the packages named: ``aura -R <packages>``
- Packages named and all deps (recursive): ``aura -Rr <packages>``


Local Package Backups
~~~~~~~~~~~~~~~~~~~~~

The state of locally installed packages may be recorded and restored
at a later date.

Usage:

- Store a snapshot of all installed packages: ``aura -B``

   - This record is stored in :file:`/var/cache/aura/states`.
   - Filenames are of the form: ``YYYY.MM.MonthName.DD.HH.MM``.
   - The data itself is stored as JSON to ease use by other
     tools.

- Restore a snapshot: ``aura -Br``

.. code-block:: javascript

   { "date": "2014-04-09",
     "time": "20:00",
     "packages": [ { "pkgname": "alsa-lib",
                     "version": "1.0.27.2-1" },
                   // more packages here
                 ]
   }

.. _other:

Other
-----

.. _dependency-resolution:

Dependency Resolution
~~~~~~~~~~~~~~~~~~~~~

-  AUR dependencies are no longer resolved through PKGBUILD bash
   parsing. The AUR 3.x API includes the necessary dependency
   information.

-  **Resolution Successful**: Data in the form is yielded. These are
   groups of packages that may be built and installed simultaneously.
   That is, they are not interdependent in any way.

-  **Version Conflicts**:

-  Dependency resolution fails and the build does not continue.

-  The user is shown the chart below so it is clear what dependencies
   from what packages are causing issues.

-  All packages that had dependency issues are shown.

-  Supplying the ``--json`` flag will output this data as JSON for
   capture by other programs.


.. code-block:: bash

   +----------+--------+----------+---------+
   | Dep Name | Parent | Status   | Version |
   +==========+========+==========+=========+
   | foo      | None   | Local    | 1.2.3   |
   | foo      | bar    | Incoming | < 1.2.3 |
   | foo      | baz    | Incoming | > 1.2.3 |
   +----------+--------+----------+---------+
   | curl     | git    | Local    | 7.36.0  |
   | curl     | pacman | Incoming | 7.37.0  |
   +----------+--------+----------+---------+
   | lua      | vlc    | Incoming | 5.2.3   |
   | lua      | conky  | Incoming | 5.2.2   |
   +----------+--------+----------+---------+

.. code-block:: javascript

   // As JSON:
   { [ { "Name": "foo",
         "Local": { "Parent": "None",
                    "Version": "1.2.3" },
         "Incoming": [ { "Parent": "bar",
                         "Version": "< 1.2.3" },
                       { "Parent": "baz",
                         "Version": "> 1.2.3" }
                     ]
       },
       { "Name": "curl",
         "Local": { "Parent": "git"
                    "Version": "7.36.0" },
         "Incoming": [ { "Parent": "pacman",
                         "Version": "7.37.0" }
                     ]
       },
       { "Name": "lua",
         "Local": "None",
         "Incoming": [ { "Parent": "vlc",
                         "Version": "5.2.3" },
                         { "Parent": "conky",
                           "Version": "5.2.2" }
                     ]
       }
     ]
   }

Dependency Information Output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Information for all immediate dependencies for any given package can
   be output in human-readable format by default with ``-{A,S}d``.

-  Adding ``--recursive`` will yield all dependencies and *their*
   dependencies as well.

-  Adding ``--json`` will output this information in JSON for use by
   other software that may sit on top of Aura.

Concurrent Package Building
~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Package data is returned from dependency checking in the form
   ``[[Package]]`` (see :ref:`dependency-resolution`). Each sublist of
   packages have no interdependencies, so they are built concurrent to
   each other and then installed as a block.

.. _pkginfo:

PkgInfo
~~~~~~~

Package searching and Info lookup algorithms work with ``PkgInfo`` data.
It holds:

- Repository name
- Package name
- Version
- Description
- Architecture
- URL
- Licenses
- “Provides”
- Dependencies
- “Conflicts With”
- Maintainer
- Optional fields (provided as ``[(Text,Text)]``):

   - Download/Install sizes
   - Group
   - Votes
   - GPG information
   - etc.

Abnormal Termination
~~~~~~~~~~~~~~~~~~~~

Users can halt Aura with ``Ctrl-d``. The message ``Stopping Aura...`` is
shown. All temporary files in use are cleared here.

Colour Output
~~~~~~~~~~~~~

All output to terminal (save JSON data) is output in colour where
appropriate. The user can disable this with ``--no-color{ur,r}``.

Usage Tips
~~~~~~~~~~

The user is shown usage tips when waiting for dependencies to resolve,
etc. A number of tips are Aura-centric, but distro-specific ones can be
defined in :ref:`auraconf`.

.. todo:: Decide frequenc and what command(s) cause these tips to appear.

Plugins
-------

Like XMonad, behaviour is built around hooks/plugins that are themselves
written in Haskell. Each Linux distribution writes and provides to
:ref:`auraconf` functions that fill certain type/behaviour requirements
as explained below.

.. _auraconf:

AuraConf
~~~~~~~~

.. todo:: document location of Aura's configuration file.

AuraConf is Aura’s configuration file.  Here, distributions and users can add
Hooks to define custom behaviour for their native packaging system.
The command ``aura --recompile`` rebuilds Aura with new Hooks.
Also, the following paths can be defined in this file:

- Package cache.
- Aura log file.
- Default build directory.
- Mirror URLs for binary downloads.
- TODO: What else?

Package Typeclass Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each Hook family (as described below) operates with one type of package.
Any package type has to implement the `Package` typeclass. It takes
the following shape:

.. code-block:: haskell

   class Package p where
     -- Converts a package name to its ADT form. Upon failure,
     -- yields its name wrapped in a `Left`.
     package :: Text -> IO (Either Text p)

     -- All Packages must be able to present their prime information
     -- in a standard way for Aura output functions.
     render :: p -> PkgInfo

Hooks ADT
~~~~~~~~~
Hooks are passed through Aura as an ADT of functions.

.. code-block:: haskell

   {-# LANGUAGE RankNTypes #-}

   data Hooks p = Hooks { info   :: Package p => Text -> IO [p]
                        , search :: Package p => Text -> IO [p]
                        , -- more to come
                        }

Aesthetics
----------

Size Information
~~~~~~~~~~~~~~~~
If ``--verbose-size`` is passed to Aura, the following information is
displayed before installation from the official repositories (may not be
possible for AUR)

.. code-block::

   Total download size: xx MiB
   Net upgrade size   : xx MiB

Localisation
~~~~~~~~~~~~

.. todo::
   Document exactly which environment variables are relevant. Perhaps $LANG?

Aura is available for use in multiple languages. Language can be set via
environment variables or by using Aura flags that correspond to that
language. Note that use of a flag will override whatever environment
variable is set. Each language has an English name and its native
equivalent (accents and other non-ascii characters are compatible). For
example:

- ``--croatian`` and ``--hrvatski``

- ``--french`` and ``--français``

.. _version-information:

Version Information When Upgrading
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Whenever a package needs an upgrade, and ``--verbose-update`` is passed to
Aura, then a detailed chart is produced, as described below.

The coloured part is denoted with ``<colour></colour>`` tags, enclosing the
text to colourise such as <colour>text to colourise</colour>.

``--verbose-update`` implies ``--verbose-size``.

New Package Dependency Needed
*****************************

.. code-block::

   == New package needed:
   repository/package        1.0-1            (required by xxx) (Net change: ±xx MiB)

New Package Release
*******************

.. code-block::

   == New package release:
   repository/package        1.0-1    -->    1.0-<green>2</green> (Net change: ±xx MiB)

New Package Version
*******************

.. code-block::

   == New package version:
   repository/package        1.0-1    -->    1.<green>2-1</green> (Net change: ±xx MiB)

Aura Versioning
~~~~~~~~~~~~~~~

-  Aura uses `Semantic Versioning`_, meaning it’s version numbers are of
   the form ``MAJOR.MINOR.PATCH``.

Haskell Requirements
--------------------

Strings
~~~~~~~

All Strings are represented as from ``Data.Text``. This is available in
the ``text`` package from Hackage. The following language pragma should
be used where appropriate for String literals being converted to
automatically::

   {-# LANGUAGE OverloadedStrings #-}

JSON Data
~~~~~~~~~

All JSON input and output is handled through the ``aeson`` and
``aeson-pretty`` packages.

Parsing
~~~~~~~

.. todo:: Decide between Parsec and Attoparsec

All parsing is done with Parsec. Regular Expressions are no longer
used anywhere in Aura.

Other Libraries
~~~~~~~~~~~~~~~

Information on other Hackage libraries used in Aura can be found
`here`_.

Package Requirements
--------------------

Aura must be available in the following forms:

``haskell-aura``
   An AUR package pulled from Hackage, contains only the Aura “shell” layer.
   The user must install another package to get the Arch Linux Hooks, and then
   build the executable themselves.

``aura``
   Official Arch-flavoured Aura, built and configured in a cabal sandbox.
   ``cabal-install`` is the only Haskell related dependency.

``haskell-aura-git``
   Most recent version of Aura, as found on its source repository.

``aura-legacy``
   A static copy of Aura 1. Has Haskell dependencies.

Arch Linux Specifics
====================

ABS Package Building/Installation
---------------------------------

-  There is no longer a ``-M`` option. All ABS package interaction is
   done through ``-S``.

-  Installs prebuilt binaries available from Arch servers by default.

-  Build options:

-  If the user specifies ``--build``, the package will be built manually
   via the ABS.

AUR Package Building/Installation
---------------------------------

-  Builds manually by default, as there is no prebuilt alternative for
   the AUR (by design).

PKGBUILD/Additional Build-file Editing
--------------------------------------

-  Support for ``customizepkg`` is dropped, as AUR 3.x provides
   dependency information via its API.

-  Users can edit included ``.install`` files and the **behaviour** of
   PKGBUILDs with ``--edit``. This is done after dependency checks have
   been made via the data from the AUR API. Users are urged *not* to
   edit dependencies at this point, as only ``makepkg``, not Aura, will
   know about the changes.

-  If you do want to build a package with different dependencies,
   consider whether there is value in creating your own forked package
   for the AUR (named ``foo-legacy``, etc.). Others may benefit from
   your effort.

-  If you are trying to fix a broken package, rather than circumventing
   the problem by building manually with ``makepkg``, please contact the
   maintainer.

AUR Interaction
---------------

-  AUR API calls are moved out of Aura and into a new Hackage package
   ``aur`` (exposing the ``Linux.Arch.Aur.*`` modules).

-  It provides conversions to and from JSON data and Haskell data.

-  This is preparation for future versions of Aura that allow use in
   other Linux distributions by swapping out sections of their back-end
   (with modules like ``Linux.Debian.Repo`` etc.)

Coding Standards
================

Record Syntax
-------------

When using record syntax for ADTs, function names should be suffixed
with “Of” to reflect their noun-like nature::

   data Package = Package { nameOf    :: String
                          , versionOf :: Version
                          , depsOf    :: [Package] }
                          deriving (Eq, Show)

.. _Aura: https://github.com/fosskers/aura
.. _attemping to create universal standards can be problematic: http://www.xkcd.com/927/
.. _Semantic Versioning: http://semver.org/
.. _here: https://github.com/fosskers/aura/issues/223
