//! A layer over the `alpm` library to aid with common tasks.

use alpm::{Alpm, Package, PackageReason};

/// The default filepath of the Pacman configuration.
pub const DEFAULT_PAC_CONF: &str = "/etc/pacman.conf";

// TODO Handle the other potential default locations.
/// The default filepath of the Makepkg configuration.
pub const DEFAULT_MAKEPKG_CONF: &str = "/etc/makepkg.conf";

#[derive(Debug)]
pub enum Error {
    Alpm(alpm::Error),
}

/// All orphaned packages.
///
/// An orphan is a package that was installed as a dependency, but whose parent
/// package is no longer installed.
pub fn orphans(alpm: &Alpm) -> Vec<Package> {
    alpm.localdb()
        .pkgs()
        .iter()
        .filter(|p| {
            p.reason() == PackageReason::Depend
                && p.required_by().is_empty()
                && p.optional_for().is_empty()
        })
        .collect()
}
