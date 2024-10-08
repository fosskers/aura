language-name = 한국어

# AUR Packages (-A)
A-install-deps = 종속성 확인 중...
A-install-repo-pkgs = 리포지토리 종속성:
A-install-aur-pkgs = AUR 패키지:
# A-install-path-comp = Failed to extract final component of: { $path }
A-install-ignored = { $file } 은(는) 무시됨으로 표시됩니다. 설치를 하시겠습니까?

# A-build-prep = Preparing build directories...
A-build-pkg = { $pkg } 빌드 중...
# A-build-diff = Display diffs of build files?
# A-build-hotedit-pkgbuild = Edit the PKGBUILD?
A-build-hotedit-install = .install 파일을 수정하시겠습니까?
A-build-fail = 빌드 실패
# A-build-e-pkgctl = Building within an isolated chroot failed.
A-build-e-makepkg = makepkg를 실패했습니다.
# A-build-e-edit = Failed to edit: { $file }
# A-build-e-tarball = Failed to move: { $file }
# A-build-e-filename = Failed to extract filename from: { $file }
# A-build-e-copies = Failed to copy build files.
# A-build-e-perm = Failed to set file permissions for: { $dir }
# A-build-pkglist = Failed to determine makepkg output paths from: { $dir }
A-build-pull = 최신 버전 'git pull'을 실패했습니다.
A-build-continue = 계속하시겠습니까?

A-i-repo = 리포지토리
A-i-version = 버전
A-i-status = AUR 상태
A-i-maintainer = 관리자
A-i-proj-url = 프로젝트 URL
A-i-aur-url = AUR URL
A-i-license = 라이센스
# A-i-group = Groups
# A-i-provides = Provides
A-i-depends = 종속 패키지
A-i-make = 빌드 종속성
# A-i-opt = Optional Deps
# A-i-check = Check Deps
A-i-votes = 투표
A-i-pop = 인기
A-i-desc = 개요
# A-i-keywords = Keywords
# A-i-submitted = Submitted
# A-i-updated = Updated

A-u-fetch-info = 패키지 정보 가져오는 중...
A-u-comparing = 패키지 버전 비교 중...
A-u-no-upgrades = 업그레이드가 필요하지 않습니다.
A-u-to-upgrade = 업그레이드할 AUR 패키지:
# A-u-git = VCS packages to rebuild:

# A-w = Cloning { $package }...

# A-y-refreshing = Refreshing local clones of known AUR packages...
# A-y-pulling = Pulling latest commits

# Snapshots (-B)
B-saved = 패키지 상태가 저장되었습니다.
# B-saved = Saved package state.
# B-clean = Remove stale snapshots?
# B-none = No usable snapshots found.
# B-select = Select a snapshot to restore:

# Cache (-C)
# C-size = Current cache size: { $size }

# C-b-file = { $target } already exists and is not a directory.
# C-b-nonempty = Target { $target } exists but is not empty!
C-b-target = 캐시를 백업하는 중 { $target }
# C-b-curr = Failed to read current directory.

# C-i-latest = Latest
# C-i-created = Created
# C-i-installed = installed
# C-i-sig = Signature
# C-i-size = Tarball Size
# C-i-avail = Available Versions

C-c-keep = 각 패키지 파일에 대해 { $pkgs } 개의 파일이 유지되어야 합니다. 나머지는 모두 삭제됩니다.
C-c-freed = { $bytes } 정리되었습니다.

C-downgrade-which = 어느 { $pkg } 버전을 설치하시겠습니까?

# C-y-no-work = Package cache already synchronized.
# C-t-invalids = Removing invalid package tarballs.

# Logs (-L)
L-first = 최초 설치
L-upgrades = 업그레이드 횟수
L-recent = 근황
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
env-pconf = pacman.conf 파일 분석에 실패했습니다.

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
# common-yes = Yes
# common-no = No
common-name = 이름
# common-done = Done.
# common-no-packages = No packages specified.
# common-no-work = Nothing to do.
# common-cancelled = Action cancelled.
# common-replace = You can delete { $old } in favour of { $new }.

# Misc.
# proceed = Proceed?
# proceed-affirmative = y
# proceed-affirmative-alt = Y
# proceed-negative = n
