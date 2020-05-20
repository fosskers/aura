# SUDO USAGE

Here are all the PKGBUILDs which contain `sudo` in some way. If you are the maintainer
of one of these packages, please fix them.

## Calling `pacman` directly

Packages which call `sudo pacman` manually, bypassing the `depends` array:

- akiee
- myget

## Installing `python` dependencies

Packages which attempt to install python deps manually instead of using
the `depends` array:

- coala
- svkm

## Building python packages

Packages that do `sudo python2 ./setup.py install` (or similar):

- camimporter
- phpsh
- specmatch

## Installing `npm` dependencies

Packages which attempt to install Javascript deps manually:

- tutanota-electron-git

## Kernel Modules

There seem to be a number of packages involving kernel modules. These call
`modprobed-db`, and claim to need `sudo`:

- linux-bfq-mq
- linux-bfq-mq-git
- linux-bld
- linux-ck
- linux-ck-vfiomsitest
- linux-clear
- linux-gc
- linux-lqx
- linux-lts-ck
- linux-pf
- linux-pf-lts
- linux-rt-bfq
- linux-uksm
- linux49-lqx

## Sudoing into your filesystem

PKGBUILDs which attempt to manipulate existing files on your filesystem:

- djinni (also creates symlinks)
- howdy
- safeaur
- transfer-cli
- waterfox-kde-bin
- ya4r

## Manually copying files into your filesystem

Packages which `sudo cp` some of their files to their target destinations manually:

- aphrodite
- archibold
- epitarendu
- obs-gnome-screencast
- python-treecorr

## Creating symlinks in your filesystem

- gamehub
- owo-cli
- tempest

## Unnecessary `sudo` usage

These seem to be benign, but still aren't necessary and should be corrected.

- arch-security
- archfetch
- benelib
- complx-git
- dtrace-utils
- epifortune
- focus-git
- git-standup-git
- herwig
- jgb-black
- jgd-cobaltblue
- jgd-springawakening
- klicknmenu
- libdtrace-ctf
- libflate
- lightdm-enso-os-greeter-git
- lolang
- openfoam-esi-1712
- pgpool-ii
- pointcarrefs
- python-bintrees-git
- rana-git
- rscheme (also does a bunch of `mount` calls!)
- timegen
- twg
- vlc-youtube

## Otherwise Strange

`slimdns-git` contains:

```bash
# Create the postgresql-user (if not exist)
sudo -i -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='slimdns'" | grep -q 1 || createuser -D -R -S slimdns; sudo -i -u postgres psql postgres -tAc "ALTER USER slimdns WITH PASSWORD '$_dbuser_passwd';"

# And create the database if it doesn't excist
sudo -i -u postgres psql postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='slimdns'" | grep -q 1 || sudo -i -u postgres psql postgres -tAc "SELECT 1 FROM pg_database WHERE datname = 'slimdns'" | grep -q 1 || psql -U postgres -c "CREATE DATABASE slimdns OWNER slimdns;"
```

`httpjs-git` contains:

```bash
echo "Fixing https://github.com/HTTPjs/HTTPjs/issues/37"
sudo setcap 'cap_net_bind_service=+ep' "$pkgdir/var/lib/httpjs/httpjs"
sudo setcap 'cap_net_bind_service=+ep' "$pkgdir/usr/bin/httpjs"
```

`hgsreceiver-bin` contains:

```bash
sudo dmidecode -t 1 | grep UUID | tr A-z a-z | tr -d - | cut -c8-80 > opt/hpremote/registration/H264
```

`fabaria` contains a `chmod 777`:

```bash
post_install() {
  sudo chmod 777 /usr/bin/fabaria
}
```

`brother-dcp7040` contains:

```bash
# Applying both commands from archasaurusrex comment
echo DCP7040 | sudo tee $pkgdir/usr/share/brother/inf/brPrintList
chown daemon:lp $pkgdir/usr/share/brother/inf/brDCP7040rc
```
