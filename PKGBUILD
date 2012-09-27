# Maintainer: Colin Woodbury <colingw@gmail.com>
_hkgname=aura
pkgname=aura
pkgver=0.10.0.0
pkgrel=1
pkgdesc="A package manager for Arch Linux and the AUR written in Haskell."
url="https://github.com/fosskers/aura"
license=('GPL-3')
arch=('i686' 'x86_64')
makedepends=('ghc' 'haskell-regex-base' 'haskell-regex-posix' 'haskell-json'
             'haskell-curl')
depends=('gmp' 'pacman')
options=('strip')
source=(https://github.com/downloads/fosskers/aura/${_hkgname}-${pkgver}.tar.gz)
md5sums=('a5a66cb66943ae7673d31b6ccdfe728c')
build() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} -O
    runhaskell Setup build

    # Installing man page
    mkdir -p "$pkgdir/usr/share/man/man8/"
    install -m 644 aura.8 "$pkgdir/usr/share/man/man8/aura.8"
}
package() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup copy --destdir=${pkgdir}
}
