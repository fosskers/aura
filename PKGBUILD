# Maintainer: Colin Woodbury <colingw@gmail.com>
_hkgname=aura
pkgname=aura
pkgver=1.3.0.0
pkgrel=2
pkgdesc="A secure package manager for Arch Linux and the AUR written in Haskell."
url="https://github.com/fosskers/aura"
license=('GPL-3')
arch=('i686' 'x86_64')
depends=('gmp' 'pacman' 'pcre' 'abs')
makedepends=('ghc' 'haskell-regex-base' 'haskell-regex-pcre-builtin'
             'haskell-mtl' 'haskell-parsec' 'haskell-transformers'
             'haskell-temporary' 'haskell-split' 'haskell-aur-git'
             'haskell-text' 'haskell-wreq' 'haskell-lens')
optdepends=('powerpill:    For faster repository downloads.'
            'customizepkg: For auto-editing of PKGBUILDs.'
            'aur-git:      AUR package completions for zsh.')
provides=('aura')
conflicts=('aura-git' 'aura-bin')
options=('strip')
source=(https://bitbucket.org/fosskers/aura/downloads/${_hkgname}-${pkgver}.tar.gz)
md5sums=('3cd3c612c78c61dd43bc7472d998c582')

build() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} -O
    runhaskell Setup build
}

package() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup copy --destdir=${pkgdir}

    # Installing man page
    mkdir -p "$pkgdir/usr/share/man/man8/"
    install -m 644 aura.8 "$pkgdir/usr/share/man/man8/aura.8"

    # Installing bash completions
    mkdir -p "$pkgdir/usr/share/bash-completion/completions/"
    install -m 644 completions/bashcompletion.sh "$pkgdir/usr/share/bash-completion/completions/aura"

    # Installing zsh completions
    mkdir -p "$pkgdir/usr/share/zsh/site-functions/"
    install -m 644 completions/_aura "$pkgdir/usr/share/zsh/site-functions/_aura"

    # Directory for storing PKGBUILDs
    mkdir -p "$pkgdir/var/cache/aura/pkgbuilds"

    # Directory for storing source packages
    mkdir -p "$pkgdir/var/cache/aura/src"

    # Directory for storing installed package states
    mkdir -p "$pkgdir/var/cache/aura/states"
}
