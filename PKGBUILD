# Maintainer: Colin Woodbury <colingw@gmail.com>
_hkgname=aura
pkgname=aura
pkgver=1.0.3.0
pkgrel=1
pkgdesc="A package manager for Arch Linux and the AUR written in Haskell."
url="https://github.com/fosskers/aura"
license=('GPL-3')
arch=('i686' 'x86_64')
makedepends=('ghc' 'haskell-regex-base' 'haskell-regex-pcre' 'haskell-json'
             'haskell-curl')
depends=('gmp' 'pacman')
optdepends=('pacman-color: For coloured pacman output in Aura.')
options=('strip')
source=(https://github.com/downloads/fosskers/aura/${_hkgname}-${pkgver}.tar.gz)
md5sums=('54632514b5844a48f7f2f513d79a8c7d')
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
