language-name = English

# AUR Packages (-A)
A-install-unreal = { $pkg } is not a real package.
A-install-cloning = Cloning new packages...
A-install-pulling = Pulling known packages...

A-build-prep = Preparing build directories...
A-build-fail = Package failed to build, citing:

A-i-repo = Repository
A-i-version = Version
A-i-status = AUR Status
A-i-maintainer = Maintainer
A-i-proj-url = Project URL
A-i-aur-url = AUR URL
A-i-license = License
A-i-group = Groups
A-i-provides = Provides
A-i-depends = Depends On
A-i-make = Make Deps
A-i-opt = Optional Deps
A-i-check = Check Deps
A-i-votes = Votes
A-i-pop = Popularity
A-i-desc = Description
A-i-keywords = Keywords
A-i-submitted = Submitted
A-i-updated = Updated

A-w = Cloning { $package }...
A-w-fail = Failed to clone the following packages:

A-y = Failed to pull the following packages:

# Snapshots (-B)
# saveState_1
B-saved = Saved package state.
B-clean = Remove stale snapshots?
B-none = No usable snapshots found.
B-select = Select a snapshot to restore:

# Cache (-C)
C-size = Current cache size: { $size }

C-b-file = { $target } already exists and is not a directory.
C-b-nonempty = Target { $target } exists but is not empty!
# backupCache_4
C-b-target = Backing up cache to { $target }

C-i-latest = Latest
C-i-created = Created
C-i-installed = installed
C-i-sig = Signature
C-i-size = Tarball Size
C-i-avail = Available Versions

# cleanCache_3 + 4
C-c-keep = { $pkgs } of each package file will be kept. The rest will be deleted.
C-c-freed = { $bytes } freed.

# getDowngradeChoice_1
C-downgrade-which = What version of { $pkg } do you want?

C-y-no-work = Package cache already synchronized.
C-y-which-cache = Which cache should receive the downloaded tarballs?
C-t-invalids = Removing invalid package tarballs.

# Orphans (-O)
O-abandon = The following orphans and their dependencies will be removed:
O-adopt = { $package } now marked as explicitly installed.

# Logs (-L)
L-first = First Install
L-upgrades = Upgrades
L-recent = Recent Actions

# System Validation (check)
check-start = Validating your system.
check-snapshots = Package Snapshots
check-snapshots-unreadable = Unable to read snapshot path { $path }
check-snapshot-usable = All snapshots have corresponding tarballs?
check-snapshot-usable-fix = Fix: You can remove old/unusable snapshots with { $command }
check-cache = Package Tarball Caches
check-cache-unreadable = Unable to read cache path { $path }
check-cache-exists = All specified caches exist?
check-cache-tarballs = All tarballs valid?
check-cache-tarballs-fix = Fix: You can remove invalid tarballs with { $command }
check-cache-missing = Every installed package has a tarball?
check-cache-missing-fix = Fix: You can download missing tarballs with { $command }

# Common Fields
common-yes = Yes
common-no = No
common-none = None
common-name = Name
common-done = Done.
common-total = Total
common-no-packages = No packages specified.
common-no-valid = No valid packages specified.
common-no-work = Nothing to do.
common-cancelled = Action cancelled.

# Misc.
proceed = Proceed?
proceed-yes = [Y/n]
proceed-no = [y/N]
continue = Continue?
