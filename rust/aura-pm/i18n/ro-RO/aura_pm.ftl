language-name = Română

# AUR Packages (-A)
A-install-deps = Se determin dependențele...
A-install-repo-pkgs = Dependențe din repertorii:
A-install-aur-pkgs = Pachete din AUR:
# A-install-path-comp = Failed to extract final component of: { $path }
A-install-ignored = { $file } e marcat ca ignorat. Să se instaleze oricum?

# A-build-prep = Preparing build directories...
A-build-pkg = Se compilează { $pkg }...
# A-build-diff = Display diffs of build files?
# A-build-hotedit-pkgbuild = Edit the PKGBUILD?
A-build-hotedit-install = Doriți să modificați fișierul .install?
A-build-fail = Compilare nereușită.
# A-build-e-pkgctl = Building within an isolated chroot failed.
A-build-e-makepkg = A fost o problemă cu makepkg.
# A-build-e-edit = Failed to edit: { $file }
# A-build-e-tarball = Failed to move: { $file }
# A-build-e-filename = Failed to extract filename from: { $file }
# A-build-e-copies = Failed to copy build files.
# A-build-e-perm = Failed to set file permissions for: { $dir }
# A-build-pkglist = Failed to determine makepkg output paths from: { $dir }
A-build-pull = Nu a reușit 'git pull' să descarce cele mai recente actualizări.
A-build-continue = Doriți oricum să continuați?

A-i-repo = Repertoriu
A-i-version = Versiune
A-i-status = Stare AUR
A-i-maintainer = Întreținător
A-i-proj-url = URL al proiectului
A-i-aur-url = URL AUR
A-i-license = Licență
# A-i-group = Groups
# A-i-provides = Provides
A-i-depends = Depinde de
A-i-make = Dependențe de compilare
# A-i-opt = Optional Deps
# A-i-check = Check Deps
A-i-votes = Voturi
A-i-pop = Popularitate
A-i-desc = Descriere
# A-i-keywords = Keywords
# A-i-submitted = Submitted
# A-i-updated = Updated

A-u-fetch-info = Se obțin informații despre pachete...
A-u-comparing = Se compar versiunile pacheturilor...
A-u-no-upgrades = Nu e nevoie să se actualizeze nici un pachet din AUR.
A-u-to-upgrade = Pachete din AUR de actualizat:
# A-u-git = VCS packages to rebuild:

# A-w = Cloning { $package }...

# A-y-refreshing = Refreshing local clones of known AUR packages...
# A-y-pulling = Pulling latest commits

# Snapshots (-B)
B-saved = Stare de pachete salvată.
# B-clean = Remove stale snapshots?
# B-none = No usable snapshots found.
# B-select = Select a snapshot to restore:

# Cache (-C)
# C-size = Current cache size: { $size }

# C-b-file = { $target } already exists and is not a directory.
# C-b-nonempty = Target { $target } exists but is not empty!
C-b-target = Se copiază cache-ul de rezervă către { $target }
# C-b-curr = Failed to read current directory.

# C-i-latest = Latest
# C-i-created = Created
# C-i-installed = installed
# C-i-sig = Signature
# C-i-size = Tarball Size
# C-i-avail = Available Versions

C-c-keep = { $pkgs } din fiecare fișier de pachet vor fi păstrate. Restul va fi șters.
# cleanCache_8 but remove "megabytes".
C-c-freed = { $bytes } eliberat.

C-downgrade-which = Care versiune al pachetului { $pkg } o doriți?

# C-y-no-work = Package cache already synchronized.
# C-t-invalids = Removing invalid package tarballs.

# Logs (-L)
L-first = Prima instalare
L-upgrades = Actualizări
L-recent = Acțiuni Recente
# L-search-err = Searching your logs via { $cmd } failed.
# L-view-err = Failed to open your ALPM log.

# Opening Pages (open)
# open-err = Failed to open { $url }.

# System Statistics (stats)
# stats-local = Failed to load language data.
# stats-host = Host
# stats-user = User
# stats-distro = Distribution
# stats-editor = Editor
# stats-pkgs = Installed packages
# stats-aura-cache = Aura Package Cache
# stats-pacman-cache = Pacman Package Cache
# stats-aura-build = Aura Build Cache
# stats-tmp = /tmp Directory

# System Validation (check)
# check-start = Validating your system.
# check-missing-exec = Fix: Please install { $exec } and/or ensure it's on your PATH.
# check-env = Environment
# check-env-editor = EDITOR variable set?
# check-env-editor-exec = EDITOR value ({ $exec }) is executable?
# check-env-editor-vi = Backup editor vi is executable?
# check-env-exec = { $exec } installed and executable?
# check-env-lang = { $cmd } contains LANG value? ({ $lang })
# check-env-lang-fix = Update your { $file } to include { $lang }.
# check-env-lang-fix2 = Set your LANG variable!
# check-env-lang-known = Aura is localised to your LANG?
# check-env-java-bin = Java tooling installed?
# check-env-java-bin-fix = Considering installing { $pkg }.
# check-env-java-set = Java environment set?
# check-env-java-set-fix = See { $cmd }.
# check-pconf = Pacman Configuration (/etc/pacman.conf)
# check-pconf-par = Parallel downloads activated?
# check-pconf-par-fix = { $setting } is off, or set to 1. Set { $set } for faster tarball fetching.
# check-pconf-ignores = No overlapping ignored packages?
# check-pconf-ignores-fix = The following packages are ignored in both pacman.conf and aura.toml: { $pkgs }
# check-pconf-pacnew = All .pacnew files accounted for?
# check-pconf-pacnew-broken = Error: Call to { $fd } utterly failed.

# check-pconf-pacnew-old = { $path } is older than its .pacnew by { $days ->
#    [one] 1 day.
#   *[many] {$days} days.
# }

# check-aconf = Aura Configuration
# check-aconf-aura-exists = Aura config file exists?
# check-aconf-aura-exists-fix = Fix: Consider { $cmd }
# check-aconf-aura-parse = Aura config file can be parsed?
# check-aconf-old-dirs = No old Aura directories exist?
# check-aconf-old-conf = No old Aura config files exist?
# check-mconf = Makepkg Configuration ({ $path })
# check-mconf-packager = PACKAGER set?
# check-mconf-packager-fix = Fix: Set { $cmd } within { $path }
# check-snapshots = Package Snapshots
# check-snapshot-usable = All snapshots have corresponding tarballs?
# check-snapshot-usable-fix = Fix: You can remove old/unusable snapshots with { $command }
# check-cache = Package Tarball Caches
# check-cache-exists = All specified caches exist?
# check-cache-tarballs = All tarballs valid?
# check-cache-tarballs-fix = Fix: You can remove invalid tarballs with { $command }
# check-cache-missing = Every installed official package has a tarball?
# check-cache-missing-fix = Fix: You can download missing official tarballs with { $command }
# check-cache-missing-for = Every installed AUR package has a tarball?
# check-cache-missing-for-fix = Fix: View the missing packages with { $cmd } and reinstall them manually.
# check-pkgs = Package Status
# check-pkgs-old = All explicitly installed, non-dep packages are up to date?
# check-pkgs-old-warn = { $pkg } was last updated { $days } ago.
# check-pkgs-empty = All package clones are populated?
# check-pkgs-empty-fix = Fix: Delete the following directories.

# Thanks
# thanks-you = Thank you for using Aura.
# thanks-colin = Aura by Colin Woodbury, 2012 - 2024
# thanks-pacman = Thank you to the Pacman and Arch Linux teams for providing a solid foundation.
# thanks-everyone = Thank you to Aura's contributors, donators, and users.
# thanks-logo = Aura's logo by Cristiano Vitorino.
# thanks-translators = Aura is localised by:

# Configuration (conf)
# conf-toml-err = Failed to serialize current config.

# Dependencies (deps)
# deps-io = Failed to generate the dependency image.

# Runtime Environment
# env-missing-editor = Provided EDITOR is not on the PATH.
env-pconf = Nu se poate analiza fișierul pacman.conf.

# Pacman Calls
# pacman-external = A call to pacman utterly failed.
# pacman-u = A call to pacman -U failed.
# pacman-s = A call to pacman -S failed.
# pacman-misc = A call to pacman gave a non-zero exit code.

# Aura-specific Directories
# dir-mkdir = Failed to create the directory: { $dir }.
# dir-home = Unable to determine Aura's config directory.
# dir-cache = Unable to determine Aura's cache directory.

# Dependency Resolution
# dep-exist = The package { $pkg } does not exist.
# dep-exist-par = The dependency { $pkg } of { $par } does not exist.
# dep-graph = The dependency graph was somehow malformed.
# dep-cycle = Dependency cycle detected: { $cycle }
# dep-multi = There were multiple errors during dependency resolution.

# Git Operations
# git-diff = A git diff failed for: { $file }
# git-hash = Reading a git hash into Rust failed.
# git-pull = A git pull failed: { $dir }
# git-clone = A git clone failed: { $dir }
# git-io = Calling git somehow failed.

# Faur Calls
# faur-fetch = Calling the metadata server utterly failed: { $pkg }
# faur-unknown = Unknown package: { $pkg }
# faur-too-many = More results returned from Faur than expected: { $pkg }

# Common Errors
# err-alpm = Failed to open ALPM handle.
# err-config-path = Failed to determine the path to Aura's config file.
# err-curl = A CURL transation failed: { $err }
# err-file-del = Failed to delete: { $file }
# err-file-open = Failed to open file handle to: { $file }
# err-file-write = Failed to write file: { $file }
# err-json-decode = Failed to decode JSON from: { $url }
# err-json-write = Failed to write JSON to: { $file }
# err-mutex = A mutex was poisoned.
# err-pool-create = Failed to create an ALPM connection pool.
# err-pool-get = Failed to get an ALPM handle from the connection pool.
# err-read-dir = Failed to read directory: { $dir }
# err-srcinfo = Failed to parse .SRCINFO: { $file }
# err-sudo = Running Aura with sudo is not necessary.
# err-time-conv = Failed to convert a timestamp.
# err-time-format = Failed to format a time string.
# err-user-input = Failed to get user input.
# err-utf8 = A UTF-8 conversion failed.
# err-write = Somehow failed to write to stdout.

# Common Fields
common-yes = Da
# common-yes = Yes
# common-no = No
common-name = Nume
# common-done = Done.
# common-no-packages = No packages specified.
# common-no-work = Nothing to do.
# common-cancelled = Action cancelled.
# common-replace = You can delete { $old } in favour of { $new }.

# Misc.
# proceed = Proceed?
proceed-affirmative = d
proceed-affirmative-alt = D
proceed-negative = n
