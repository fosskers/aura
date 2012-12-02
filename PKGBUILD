# Maintainer: Colin Woodbury <colingw@gmail.com>
_hkgname=aura
pkgname=aura
pkgver=1.0.6.0
pkgrel=1
pkgdesc="A package manager for Arch Linux and the AUR written in Haskell."
url="https://github.com/fosskers/aura"
license=('GPL-3')
arch=('i686' 'x86_64')
depends=('gmp' 'pacman' 'ghc' 'haskell-regex-base' 'haskell-regex-pcre' 
         'haskell-curl' 'haskell-json' 'haskell-url')
optdepends=('pacman-color: For coloured pacman output in Aura.')
options=('strip')
source=(https://github.com/downloads/fosskers/aura/${_hkgname}-${pkgver}.tar.gz)
md5sums=('1efdf5cd483e8617edeea980bc158499')
build() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} -O
    runhaskell Setup build

    # Installing man page
    mkdir -p "$pkgdir/usr/share/man/man8/"
    install -m 644 aura.8 "$pkgdir/usr/share/man/man8/aura.8"

    # Directory for storing PKGBUILDs
    mkdir -p "$pkgdir/var/cache/aura/pkgbuilds"
}
package() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup copy --destdir=${pkgdir}
}
