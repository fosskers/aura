# aura-core

## Unreleased

#### Fixed

- Pulling package data for packages with an extension in the name like [genwipe.sh][wipe].

[wipe]: https://aur.archlinux.org/packages/genwipe.sh

## 0.8.0 (2024-07-31)

A final lockstep release for Aura 4.

## 0.7.0 (2024-07-04)

#### Fixed

- `base-devel` is always added as a dependency if missing from the system.
- Ignore inter-dependencies between split packages.

#### Changed

- Snapshot timestamps are now based on UTC.

#### Removed

- Removed the `Apply` trait. Use [applying](https://lib.rs/crates/applying) instead.

## 0.5.0 (2024-06-11)

#### Changed

- Various improvements to dependency graph visualisation logic.

## 0.3.0 (2024-03-19)

#### Changed

- Support for Version 3 of `alpm`.
