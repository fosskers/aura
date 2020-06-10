# Installation

Aura is software specific to [Arch Linux](https://www.archlinux.org/). It should
also work on Arch-derivatives like [Manjaro](https://manjaro.org/), but would
not be useful on any other Linux distribution.

## From the AUR

### Prebuilt Binary

It is recommended to install the [prebuilt
binary](https://aur.archlinux.org/packages/aura-bin/) of Aura:

```bash
git clone https://aur.archlinux.org/aura-bin.git
cd aura-bin
makepkg -s
sudo pacman -U <the-package-file-that-makepkg-produces>
```

### Source Package

For users who choose to build packages themselves or who already have an
established Haskell development environment, the vanilla
[aura](https://aur.archlinux.org/packages/aura) package is also available.

```bash
git clone https://aur.archlinux.org/aura.git
cd aura
makepkg -s
sudo pacman -U <the-package-file-that-makepkg-produces>
```

## Building from Source

You will need the [Stack Tool](https://docs.haskellstack.org/en/stable/README/)
for Haskell to compile Aura yourself. Then:

```bash
git clone https://github.com/fosskers/aura.git
cd aura
stack install -- aura
```

This may take a while to initially build all of Aura's dependencies. Once
complete, your `aura` binary will be available in `/home/YOU/.local/bin/`.

Keep in mind that this version of Aura won't be tracked in `pacman`'s database,
and so it will be easier to miss updates.
