language-name = English

# AUR Packages (-A)
aur-info-version = Version
aur-info-status = AUR Status
aur-info-maintainer = Maintainer
aur-info-project-url = Project URL
aur-info-license = License
aur-info-group = Groups
aur-info-provides = Provides
aur-info-depends = Depends On
aur-info-makedeps = Make Deps
aur-info-optdeps = Optional Deps
aur-info-checkdeps = Check Deps
aur-info-votes = Votes
aur-info-popularity = Popularity
aur-info-description = Description
aur-info-keywords = Keywords
aur-info-submitted = Submitted
aur-info-updated = Updated

# Snapshots (-B)
# saveState_1
snapshot-saved = Saved package state.
snapshot-clean = Remove stale snapshots?
snapshot-none = No usable snapshots found.
snapshot-select = Select a snapshot to restore:

# Cache (-C)
cache-size = Current cache size: { $size }

cache-backup-file = { $target } already exists and is not a directory.
cache-backup-nonempty = Target { $target } exists but is not empty!
# backupCache_4
cache-backup-target = Backing up cache to { $target }

cache-info-latest = Latest
cache-info-created = Created
cache-info-installed = installed
cache-info-sig = Signature
cache-info-size = Tarball Size
cache-info-avail = Available Versions

# cleanCache_3 + 4
cache-clean-keep = { $pkgs } of each package file will be kept. The rest will be deleted.
cache-clean-freed = { $bytes } freed.

# getDowngradeChoice_1
cache-downgrade-which = What version of { $pkg } do you want?

cache-refresh-no-work = Package cache already synchronized.
cache-invalids = Removing invalid package tarballs.

# Orphans (-O)
orphans-abandon = The following orphans and their dependencies will be removed:
orphans-adopt = { $package } now marked as explicitly installed.

# Logs (-L)
logs-first = First Install
logs-upgrades = Upgrades
logs-recent = Recent Actions

# System Validation (check)
check-start = Validating your system.
check-snapshots = Package Snapshots
check-snapshots-unreadable = Unable to read snapshot path { $path }
check-snapshot-usable = All snapshots have corresponding tarballs?
check-snapshot-usable-fix = Fix: You can remove old/unusable snapshots with { $command }
check-cache = Package Tarball Cache
check-cache-unreadable = Unable to read cache path { $path }
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
common-no-work = Nothing to do.

# Misc.
proceed = Proceed?
proceed-yes = [Y/n]
proceed-no = [y/N]
