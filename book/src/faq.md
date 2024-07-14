# FAQ

## Why doesn't package XYZ build?

If you're on the most recent version of Aura and are still having problems, this
could be for a few reasons:

- There is something wrong with the upstream package source files or a
  dependency.
  - Try checking the comments on the AUR page for that package: `aura -Ao <pkg>`
- The build environment of that package is somehow polluted.
  - Try deleting everything found in `~/.cache/aura/builds/<naughty-package>/`.
- You've found a real bug in Aura.
  - [Please report it!](https://github.com/fosskers/aura/issues)

## How do I debug a problem?

To display extra output during Aura's operation, pass `--log-level=info`. If
Aura is failing mysteriously somewhere, that output will at least let us know
how far it got internally. Even more detail will be printed with
`--log-level=debug`.

## Why did you rewrite Aura in Rust?

Haskell is an excellent language. However, I had specific reasons to move to
Rust:

1. To gain access to the `alpm` bindings already available for Rust.
2. To open up Aura to more potential contributors.
3. To reduce Aura's binary size.
4. To improve Aura's CLI interface and `--help` messages.
5. To improve Aura's performance.

> You could have done any of that in Haskell.

Perhaps, but I didn't want to.

See also the [original discussion](https://github.com/fosskers/aura/discussions/657).

## Why did you write a custom metadata server?

[The Faur](https://git.sr.ht/~fosskers/faur) yields the same JSON format as the
usual AUR RPC, but offers new endpoints. The primary functionality I required
was "provides"-based lookup, but for AUR packages. The simplest way to achieve
that was to write my own server, so I did.

An example of provides-based searching would be:

> I want to know all of the packages that think they are `gcc`.

We can search that like so:

```
> aura -Av gcc
aur/gcc-git 13.0.0_r197401.g33be3ee36a7-1 (15 | 0.00) 
    The GNU Compiler Collection - C and C++ frontends (git version)
aur/gccrs-git 14.0.1_r213484.g646046091b7-1 (2 | 0.00) 
    The GNU Compiler Collection - C and C++ frontends (git version)
aur/gcc-snapshot 15.0.1.snapshot20240707-1 (1 | 0.32) 
    The GNU Compiler Collection - C and C++ frontends (snapshot)
```

Any of these packages, if installed, "provides" the "package identity" of `gcc`.
Should another package require `gcc` as a dependency, any of these will satisfy
the resolver.

Having Aura communicate with a server under my control also helps reduce the
load on the AUR RPC.

Note that the data in the Faur is at any time no more than 1 hour older than
that found on the AUR.
