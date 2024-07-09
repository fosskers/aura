# Installation

Aura is software specific to [Arch Linux](https://www.archlinux.org/). It should
also work on Arch-derivatives like [Manjaro](https://manjaro.org/), but would
not be useful on any other Linux distribution.

There are several `aura` packages available. Aura was originally written in
Haskell, but a [port to Rust](https://github.com/fosskers/aura/discussions/657)
was released in 2024 as the `4.x` series.

## From the AUR

### The Tagged Release

[The recommended package](https://aur.archlinux.org/packages/aura) is simply
named `aura`. It uses `cargo` to build a fresh binary on your machine, based on
releases made to Rust's [crates.io](https://crates.io/crates/aura-pm).

```bash
git clone https://aur.archlinux.org/aura.git
cd aura
makepkg -s
sudo pacman -U <the-package-file-that-makepkg-produces>
```

### The `git`-based Build

If instead you'd like to directly track updates to Aura's `master` branch, install
[the `git` variant](https://aur.archlinux.org/packages/aura-git):

```bash
git clone https://aur.archlinux.org/aura-git.git
cd aura-git
makepkg -s
sudo pacman -U <the-package-file-that-makepkg-produces>
```

### The Prebuilt Binary

Finally, if you don't wish to build Aura yourself or want to avoid any trace of
Rust tooling on your machine, there is a [prebuilt
binary](https://aur.archlinux.org/packages/aura-bin/) of Aura for `x86_64`
machines:

```bash
git clone https://aur.archlinux.org/aura-bin.git
cd aura-bin
makepkg -s
sudo pacman -U <the-package-file-that-makepkg-produces>
```

## Building from Source

If you already have Rust tooling installed on your machine and/or wish to help
develop Aura, you can also install it manually:

```bash
git clone https://github.com/fosskers/aura.git
cd aura/rust
cargo install --path aura-pm
```

This will build and install the binary to `/home/YOU/.cargo/bin/`.

Keep in mind that this variant of Aura won't be tracked in `pacman`'s database,
and so it will be easier to miss updates. It also does not install completions
or other documentation files like manpages.
