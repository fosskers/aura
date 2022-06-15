language-name = English

# AUR Packages (-A)
A-install-unreal = { $pkg } is not a real package.
A-install-cloning = Cloning new packages...
A-install-pulling = Pulling known packages...
A-install-deps = Resolving dependencies...
A-install-repo-pkgs = Repository dependencies:
A-install-aur-pkgs = AUR packages:

A-build-prep = Preparing build directories...
A-build-pkg = Building { $pkg }...
A-build-diff = Display diffs of build files?
A-build-hotedit-pkgbuild = Edit the PKGBUILD?
A-build-hotedit-install = Edit the .install file?
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

# upgradeAURPkgs_1
A-u-fetch-info = Fetching package information...
# upgradeAURPkgs_2
A-u-comparing = Comparing package versions...
# upgradeAURPkgs_3
A-u-no-upgrades = No AUR package upgrades necessary.
# reportPkgsToUpgrade_1
A-u-to-upgrade = AUR packages to upgrade:
A-u-git = VCS packages to rebuild:

A-w = Cloning { $package }...
A-w-fail = Failed to clone the following packages:

A-y-refreshing = Refreshing local clones of installed AUR packages...
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

# Opening Pages (open)
open-err = Failed to open { $url }.

# System Statistics (stats)
stats-local = Failed to load language data.

# System Validation (check)
check-start = Validating your system.
check-missing-exec = Fix: Please install { $exec } and/or ensure it's on your PATH.
check-env = Environment
check-env-editor = EDITOR variable set?
check-env-editor-exec = EDITOR value ({ $exec }) is executable?
check-env-editor-vi = Backup editor vi is executable?
check-env-installed = { $exec } installed and executable?
check-pconf = Pacman Configuration
check-pconf-parallel = Parallel downloads activated?
check-pconf-parallel-fix = Your { $setting } setting is off, or set to 1. Is that intended?
check-pconf-ignores = No overlapping ignored packages?
check-pconf-ignores-fix = The following packages are ignored in both pacman.conf and aura.toml: { $pkgs }
check-pconf-pacnew = All .pacnew files accounted for?
check-pconf-pacnew-broken = Error: Call to { $fd } utterly failed.
check-pconf-pacnew-old = { $path } is older than its .pacnew by { $days } days.
check-aconf = Aura Configuration
check-aconf-aura-exists = Aura config file exists?
check-aconf-aura-exists-fix = Fix: Consider { $cmd }
check-aconf-aura-parse = Aura config file can be parsed?
check-mconf = Makepkg Configuration
check-mconf-packager = PACKAGER set?
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
check-cache-missing-fix = Fix: You can download missing official tarballs with { $command }

# Configuration (conf)
conf-toml-err = Failed to serialize current config.

# Common Errors
err-alpm = Failed to open ALPM handle.
err-pool = Failed to create an ALPM connection pool.
err-config-path = Failed to determine the path to Aura's config file.

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
