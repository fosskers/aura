language-name = English

# AUR Packages (-A)
A-install-unreal = { $pkg } is not a real package.
A-install-cloning = Cloning new packages...
A-install-pulling = Pulling known packages...
A-install-deps = Resolving dependencies...
A-install-repo-pkgs = Repository dependencies:
A-install-aur-pkgs = AUR packages:
A-install-path-comp = Failed to extract final component of: { $path }
A-install-ignored = { $file } is marked "ignored". Install anyway?

A-build-prep = Preparing build directories...
A-build-pkg = Building { $pkg }...
A-build-diff = Display diffs of build files?
A-build-hotedit-pkgbuild = Edit the PKGBUILD?
A-build-hotedit-install = Edit the .install file?
A-build-fail = Package failed to build, citing:
A-build-e-makepkg = makepkg failed.
A-build-e-edit = Failed to edit: { $file }
A-build-e-tarball = Failed to move: { $file }
A-build-e-filename = Failed to extract filename from: { $file }
A-build-e-copies = Failed to copy build files.
A-build-pkglist = Failed to determine makepkg output paths from: { $dir }
A-build-pull = Failed to pull latest commits - you may be building an old version!
A-build-continue = Continue building other packages?

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
C-b-curr = Failed to read current directory.

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
O-adopt = { $pkg } now marked as explicitly installed.
O-explicit-err = Failed to mark { $pkg } as explicitly installed.

# Logs (-L)
L-first = First Install
L-upgrades = Upgrades
L-recent = Recent Actions
L-search-err = Searching your logs via { $cmd } failed.
L-view-err = Failed to open your ALPM log.

# Opening Pages (open)
open-err = Failed to open { $url }.

# System Statistics (stats)
stats-local = Failed to load language data.
stats-host = Host
stats-user = User
stats-distro = Distribution
stats-editor = Editor
stats-pkgs = Installed packages
stats-aura-cache = Aura Package Cache
stats-pacman-cache = Pacman Package Cache
stats-aura-build = Aura Build Cache
stats-tmp = /tmp Directory

# System Validation (check)
check-start = Validating your system.
check-missing-exec = Fix: Please install { $exec } and/or ensure it's on your PATH.
check-env = Environment
check-env-editor = EDITOR variable set?
check-env-editor-exec = EDITOR value ({ $exec }) is executable?
check-env-editor-vi = Backup editor vi is executable?
check-env-installed = { $exec } installed and executable?
check-env-lang = { $cmd } contains LANG value? ({ $lang })
check-env-lang-fix = Update your { $file } to include { $lang }.
check-env-lang-fix2 = Set your LANG variable!
check-env-lang-known = Aura is localised to your LANG?
check-env-java-bin = Java tooling installed?
check-env-java-bin-fix = Considering installing { $pkg }.
check-env-java-set = Java environment set?
check-env-java-set-fix = See { $cmd }.
check-pconf = Pacman Configuration (/etc/pacman.conf)
check-pconf-par = Parallel downloads activated?
check-pconf-par-fix = { $setting } is off, or set to 1. Set { $set } for faster tarball fetching.
check-pconf-ignores = No overlapping ignored packages?
check-pconf-ignores-fix = The following packages are ignored in both pacman.conf and aura.toml: { $pkgs }
check-pconf-pacnew = All .pacnew files accounted for?
check-pconf-pacnew-broken = Error: Call to { $fd } utterly failed.

check-pconf-pacnew-old = { $path } is older than its .pacnew by { $days ->
    [one] 1 day.
   *[many] {$days} days.
}

check-aconf = Aura Configuration
check-aconf-aura-exists = Aura config file exists?
check-aconf-aura-exists-fix = Fix: Consider { $cmd }
check-aconf-aura-parse = Aura config file can be parsed?
check-aconf-old-dirs = No old Aura directories exist?
check-aconf-old-conf = No old Aura config files exist?
check-mconf = Makepkg Configuration (/etc/makepkg.conf)
check-mconf-packager = PACKAGER set?
check-mconf-packager-fix = Set { $cmd } within /etc/makepkg.conf
check-snapshots = Package Snapshots
check-snapshots-unreadable = Unable to read snapshot path: { $path }
check-snapshot-usable = All snapshots have corresponding tarballs?
check-snapshot-usable-fix = Fix: You can remove old/unusable snapshots with { $command }
check-cache = Package Tarball Caches
check-cache-unreadable = Unable to read cache path: { $path }
check-cache-exists = All specified caches exist?
check-cache-tarballs = All tarballs valid?
check-cache-tarballs-fix = Fix: You can remove invalid tarballs with { $command }
check-cache-missing = Every installed official package has a tarball?
check-cache-missing-fix = Fix: You can download missing official tarballs with { $command }
check-cache-missing-for = Every installed AUR package has a tarball?
check-cache-missing-for-fix = Fix: View the missing packages with { $cmd } and reinstall them manually.
check-pkgs = Package Status
check-pkgs-old = All explicitly installed, non-dep packages are up to date?
check-pkgs-old-warn = { $pkg } was last updated { $days } ago.

# Configuration (conf)
conf-toml-err = Failed to serialize current config.

# Runtime Environment
env-missing-editor = Provided EDITOR is not on the PATH.
env-pconf = Failed to parse your pacman.conf file.

# Pacman Calls
pacman-external = A call to pacman utterly failed.
pacman-u = A call to pacman -U failed.
pacman-s = A call to pacman -S failed.
pacman-misc = A call to pacman gave a non-zero exit code.

# Aura-specific Directories
dir-mkdir = Failed to create the directory: { $dir }.
dir-home = Unable to determine Aura's config directory.
dir-cache = Unable to determine Aura's cache directory.

# Dependency Resolution
dep-exist = The package { $pkg } does not exist.
dep-exist-par = The dependency { $pkg } of { $par } does not exist.
dep-graph = The dependency graph was somehow malformed.
dep-cycle = There was a cyclic dependency involving: { $pkg }
dep-multi = There were multiple errors during dependency resolution.

# Git Operations
git-diff = A git diff failed for: { $file }
git-hash = Reading a git hash into Rust failed.
git-pull = A git pull failed: { $dir }
git-clone = A git clone failed: { $dir }
git-io = Calling git somehow failed.

# Faur Calls
faur-fetch = Calling the metadata server utterly failed: { $pkg }
faur-unknown = Unknown package: { $pkg }
faur-too-many = More results returned from Faur than expected: { $pkg }

# Common Errors
err-alpm = Failed to open ALPM handle.
err-config-path = Failed to determine the path to Aura's config file.
err-curl = A CURL transaction failed.
err-file-del = Failed to delete: { $file }
err-file-open = Failed to open file handle to: { $file }
err-file-write = Failed to write file: { $file }
err-json-decode = Failed to decode JSON from: { $url }
err-json-write = Failed to write JSON to: { $file }
err-mutex = A mutex was poisoned.
err-none-exist = None of the specified packages exist.
err-pool-create = Failed to create an ALPM connection pool.
err-pool-get = Failed to get an ALPM handle from the connection pool.
err-read-dir = Failed to read directory: { $dir }
err-srcinfo = Failed to parse .SRCINFO: { $file }
err-sudo = Failed to raise privileges.
err-time-conv = Failed to convert a timestamp.
err-time-format = Failed to format a time string.
err-time-local = Failed to determine local time.
err-user-input = Failed to get user input.
err-utf8 = A UTF-8 conversion failed.
err-write = Somehow failed to write to stdout.

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
common-replace = You can delete { $old } in favour of { $new }.

# Misc.
proceed = Proceed?
proceed-yes = [Y/n]
proceed-no = [y/N]
