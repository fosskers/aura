language-name = Lingua Latina

# AUR Packages (-A)
A-install-deps = Inveniens requirendas...
A-install-repo-pkgs = Requirendae repositorii:
A-install-aur-pkgs = AUR sarcinae:
A-install-path-comp = Defecit extrahere ultimam partem de itere: { $path }
A-install-ignored = { $file } signatur "omissum". Visne tamen id instituere?

A-build-prep = Parans receptacula aedificatoria...
A-build-pkg = Aedificans { $pkg }...
A-build-diff = Visne videre differentiae chartarum aedificatoriarum?
A-build-hotedit-pkgbuild = Visne mutare PKGBUILD?
A-build-hotedit-install = Visne mutare .install chartam?
A-build-fail = Sarcina aedificari defecit, quod:
A-build-e-pkgctl = Defecit aedificare in remoto chroot.
A-build-e-makepkg = makepkg defecitur.
A-build-e-edit = Defecit mutare: { $file }
A-build-e-tarball = Defecit movere: { $file }
A-build-e-filename = Defecit extrahere nomen chartae ex: { $file }
A-build-e-copies = Defecit exscribere chartas aedificatas.
A-build-e-perm = Defecit statuere accessibilitatem receptaculi: { $dir } 
A-build-pkglist = Defecit invenire makepkg itinera in: { $dir }
A-build-pull = Defecit extrahere commissiones novissimas -- fortasse versionem antiquam aedificas!
A-build-continue = Visne continuare aedificare alias sarcinas?

A-i-repo = Repositorium
A-i-version = Versio
A-i-status = AUR Status
A-i-maintainer = Custos
A-i-proj-url = Facinoris URL
A-i-aur-url = AUR URL
A-i-license = Condicio
A-i-group = Circulus
A-i-provides = Dat
A-i-depends = Requirit
A-i-make = Make Requirendas
A-i-opt = Abritrarias Requirendas
A-i-check = Check Requirendas
A-i-votes = Sufragii
A-i-pop = Favor Populi
A-i-desc = Descriptio
A-i-keywords = Verbae Significantes
A-i-submitted = Summisitur
A-i-updated = Mutavitur

A-u-fetch-info = Inveniens notitia sarcinarum...
A-u-comparing = Comparans versiones sarcinarum...
A-u-no-upgrades = AUR sarcinae curandae non sunt.
A-u-to-upgrade = AUR sarcinae curandae:
A-u-git = VCS sarcinae curandae:

A-w = Simulans { $package }...

A-y-refreshing = Curans simulacros AUR sarcinarum notarum...
A-y-pulling = Extrahens commissiones novissimas

# Snapshots (-B)
B-saved = Status sarcinarum perscribit.
B-clean = Visne removere capturas obsoletas?
B-none = Captura efficax non invenit.
B-select = Eligere capturam referre:

# Cache (-C)
C-size = Magnitudo cacheae praesentis:

C-b-file = { $target } iam exsistit et receptacula non est.
C-b-nonempty = Petita { $target } exsistit sed vacuus non est!
C-b-target = Exscribens cacheam ad { $target }
C-b-curr = Defecit legere hanc receptaculam.

C-i-latest = Novissimus
C-i-created = Factus
C-i-installed = statutus
C-i-sig = Signum Formae
C-i-size = Tarball Magnitudo
C-i-avail = Versiones Parabiles

# C-c-keep = { $pkgs } of each package file will be kept. The rest will be deleted.
C-c-freed = { $bytes } liberati sunt.

C-downgrade-which = Quid versionem { $pkg } vis?

# C-y-no-work = Package cache already synchronized.
C-t-invalids = Removens tarball invalidos sarcinarum.

# # Logs (-L)
# L-first = First Install
# L-upgrades = Upgrades
L-recent = Recentes Facti
# L-search-err = Searching your logs via { $cmd } failed.
# L-view-err = Failed to open your ALPM log.

# # Opening Pages (open)
# open-err = Failed to open { $url }.

# System Statistics (stats)
# stats-local = Failed to load language data.
# stats-host = Host
stats-user = Usor
# stats-distro = Distribution
# stats-editor = Editor
stats-pkgs = Sarcinae institutae
# stats-aura-cache = Aura Package Cache
# stats-pacman-cache = Pacman Package Cache
# stats-aura-build = Aura Build Cache
stats-tmp = /tmp Receptacula

# System Validation (check)
check-start = Confirmans systema tua.
check-missing-exec = Solutio: Quaeso institue { $exec } et/aut cura ut adest in PATH tuum.
check-env = Ambitus
check-env-editor = EDITOR variabilis statuiturne?
check-env-editor-exec = EDITOR contentum ({ $exec }) exsequi potestne?
check-env-editor-vi = Alius editor vi exsequi potestne?
check-env-exec = { $exec } institutus estne et exsequi potestne?
check-env-lang = { $cmd } habet LANG contentum? ({ $lang })
check-env-lang-fix = Solutio: Muta { $file } charta tua ut habeat { $lang }.
check-env-lang-fix2 = Solutio: Statue LANG variabilis tuus!
check-env-lang-known = Aura in lingua LANG tua estne?
check-env-java-bin = Java instrumenti institui suntne?
check-env-java-bin-fix = Solutio: Considera instituere { $pkg }.
check-env-java-set = Java ambitus statuitur?
check-env-java-set-fix = Solutio: Vide { $cmd }.
check-pconf = Pacman Configuratio (/etc/pacman.conf)
check-pconf-par = Simultanei detrati constituti suntne?
check-pconf-par-fix = Solutio: { $setting } inactivus est, aut unum statuitur. Statue { $set } ut tarball extrahens celeriter est.
check-pconf-ignores = Num duplices omissae sarcinae sunt?
check-pconf-ignores-fix = Hae sarcinae et in pacman.conf et in aura.toml omissae sunt: { $pkgs }
check-pconf-pacnew = Omnes .pacnew chartae curae estne?
check-pconf-pacnew-broken = Error: Mandatum { $fd } omnino defecit.

# check-pconf-pacnew-old = { $path } is older than its .pacnew by { $days ->
#     [one] 1 day.
#    *[many] {$days} days.
# }

check-aconf = Aurae Configuratio
check-aconf-aura-exists = Charta configurationis Aurae exsistatne?
check-aconf-aura-exists-fix = Solutio: Considera { $cmd }
check-aconf-aura-parse = Charta configurationis Aurae intellegere potestne?
check-aconf-old-dirs = Nulla antiqua receptacula exsistitne?
check-aconf-old-conf = Nulla antiqua charta configurationis Aurae exsistitne?
check-mconf = Makepkg Configuratio ({ $path })
check-mconf-packager = PACKAGER statuiturne?
check-mconf-packager-fix = Solutio: Statue { $cmd } in { $path }
check-snapshots = Capturae Sarcinarum
check-snapshot-usable = Omnes capturae pares tarball tenent?
check-snapshot-usable-fix = Solutio: Potes removere antiquas/malas capturas cum { $command }
check-cache = Sarcinarum Tarball Cacheae
check-cache-exists = Omnes cacheae exsistantne?
check-cache-tarballs = Omnes tarball validine?
check-cache-tarballs-fix = Solutio: Potesne removere invalidos tarball cum { $command }
check-cache-missing = Omnes sarcinae institutae primae tarball tenentne?
check-cache-missing-fix = Solutio: Potes detrahere tarball absentes primae cum { $command }
check-cache-missing-for = Omnes AUR sarcinae institutae tarball tenentne?
check-cache-missing-for-fix = Solutio: Vide sarcinas absentes cum { $cmd } et illas rursus instituere manu.
check-pkgs = Sarcinarum Status
# check-pkgs-old = All explicitly installed, non-dep packages are up to date?
# check-pkgs-old-warn = { $pkg } was last updated { $days } ago.
# check-pkgs-empty = All package clones are populated?
# check-pkgs-empty-fix = Fix: Delete the following directories.

# Thanks
thanks-you = Gratias tibi pro utendo Auram.
thanks-colin = Aura a Colin Woodbury, 2012 - 2024
thanks-pacman = Gratias collegiis Pacman et Arch Linux pro faciendo fundamentum.
thanks-everyone = Gratias Aurae adiutoribus donatoribusque usoribusque.
thanks-logo = Aurae signum a Cristiano Vitorino.
thanks-translators = Aura translata est a:

# Configuration (conf)
conf-toml-err = Defecit redigere configurationem praesentem.

# Dependencies (deps)
deps-io = Defecit generare imagem requirendarum.

# Runtime Environment
env-missing-editor = EDITOR datum in PATH non est.
env-pconf = Defecit perspicere tua charta pacman.conf.

# Pacman Calls
pacman-external = Mandatum pacman omnino defecit.
pacman-u = Mandatum pacman -U defecit.
pacman-s = Mandatum pacman -S defecit.
pacman-misc = Mandatum pacman nuntiavit signum exiti qui nonnullum est.

# # Aura-specific Directories
dir-mkdir = Defecit creare receptaculam: { $dir }.
dir-home = Non potest invenire receptaculam configurationis Auris.
dir-cache = Non potest invenire receptaculam cacheae Auris.

# Dependency Resolution
dep-exist = Sarcina { $pkg } non exsistit.
dep-exist-par = Requirenda { $pkg } sarcinae { $par } non exsistit.
# dep-graph = The dependency graph was somehow malformed.
# dep-cycle = Dependency cycle detected: { $cycle }
dep-multi = Multi errores erant dum decretus requirendarum.

# Git Operations
git-diff = Git differentia defecitur pro: { $file }
# git-hash = Reading a git hash into Rust failed.
# git-pull = A git pull failed: { $dir }
# git-clone = A git clone failed: { $dir }
# git-io = Calling git somehow failed.

# # Faur Calls
# faur-fetch = Calling the metadata server utterly failed: { $pkg }
faur-unknown = Ignota sarcina: { $pkg }
# faur-too-many = More results returned from Faur than expected: { $pkg }

# Common Errors
# err-alpm = Failed to open ALPM handle.
err-config-path = Defecit invenire iter ad chartam configurationis Auris.
# err-curl = A CURL transaction failed: { $err }
err-file-del = Defecit perdere: { $file }
# err-file-open = Failed to open file handle to: { $file }
err-file-write = Defecit scribere chartam: { $file }
err-json-decode = Defecit scisco JSON ab: { $url }
err-json-write = Defecit scribere JSON ad: { $file }
err-mutex = Mutex venenatum est.
# err-pool-create = Failed to create an ALPM connection pool.
# err-pool-get = Failed to get an ALPM handle from the connection pool.
err-read-dir = Defecit legere receptaculam: { $dir }
# err-srcinfo = Failed to parse .SRCINFO: { $file }
err-sudo = Uti Aura cum sudo non necesse est.
# err-time-conv = Failed to convert a timestamp.
# err-time-format = Failed to format a time string.
err-user-input = Defecit acquirere scriptum usoris.
err-utf8 = UTF-8 conversio defecitur.
err-write = Ignoto errore defecit scribere ad stdout.

# Common Fields
common-yes = Volo
common-no = Nolo
common-name = Nomen
common-done = Perfectus.
common-no-packages = Nullae sarcinae scriptae est.
common-no-work = Nihil agendum est.
common-cancelled = Factum sistit.
common-replace = Potes removere { $old } pro { $new }.

# Misc.
proceed = Visne procedere?
proceed-affirmative = v
proceed-affirmative-alt = V
proceed-negative = n
