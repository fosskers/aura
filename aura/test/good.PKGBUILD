# Maintainer: Colin Woodbury <colin@fosskers.ca>
_hkgname=aura
pkgname=aura-bin
pkgver=2.4.0
pkgrel=1
pkgdesc="A secure package manager for Arch Linux and the AUR - Prebuilt binary"
url="https://github.com/fosskers/aura"
license=('GPL-3')
arch=('x86_64')
depends=('gmp' 'pacman' 'git')
optdepends=()
provides=('aura')
conflicts=('aura' 'aura-git')
options=('strip')
source=(https://github.com/fosskers/aura/releases/download/v${pkgver}/aura-${pkgver}-x86_64.tar.gz)
md5sums=('4db7c2661224b5d8d7054fff67a55d23')

package() {
    # Install aura binary
    mkdir -p "$pkgdir/usr/bin/"
    install -m 755 aura "$pkgdir/usr/bin/"

    # Installing conf file
    mkdir -p "$pkgdir/etc/"
    install -m 644 aura.conf "$pkgdir/etc/aura.conf"

    # Installing bash completions
    mkdir -p "$pkgdir/usr/share/bash-completion/completions/"
    install -m 644 bashcompletion.sh "$pkgdir/usr/share/bash-completion/completions/aura"

    # Installing zsh completions
    mkdir -p "$pkgdir/usr/share/zsh/site-functions/"
    install -m 644 _aura "$pkgdir/usr/share/zsh/site-functions/_aura"

    # Directory for storing PKGBUILDs
    mkdir -p "$pkgdir/var/cache/aura/pkgbuilds"

    # Directory for storing source packages
    mkdir -p "$pkgdir/var/cache/aura/src"

    # Directory for storing installed package states
    mkdir -p "$pkgdir/var/cache/aura/states"
}
