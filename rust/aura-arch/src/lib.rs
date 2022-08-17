//! A layer over the `alpm` library to aid with common tasks.

use alpm::{Alpm, Package, PackageReason, SigLevel};
use alpm_utils::DbListExt;
use std::path::Path;

/// All orphaned packages.
///
/// An orphan is a package that was installed as a dependency, but whose parent
/// package is no longer installed.
pub fn orphans(alpm: &Alpm) -> impl Iterator<Item = Package<'_>> {
    alpm.localdb().pkgs().into_iter().filter(|p| {
        p.reason() == PackageReason::Depend
            && p.required_by().is_empty()
            && p.optional_for().is_empty()
    })
}

/// All official packages.
pub fn officials(alpm: &Alpm) -> impl Iterator<Item = Package<'_>> {
    let syncs = alpm.syncdbs();

    alpm.localdb()
        .pkgs()
        .into_iter()
        .filter_map(move |p| syncs.pkg(p.name()).ok())
}

/// All foreign packages as an `Iterator`.
pub fn foreigns(alpm: &Alpm) -> impl Iterator<Item = Package<'_>> {
    let syncs = alpm.syncdbs();

    alpm.localdb()
        .pkgs()
        .into_iter()
        .filter(move |p| syncs.pkg(p.name()).is_err())
}

/// Does the given `Path` point to a valid tarball that can can loaded by ALPM?
pub fn is_valid_package(alpm: &Alpm, path: &Path) -> bool {
    match path.to_str() {
        None => false,
        Some(p) => path.exists() && alpm.pkg_load(p, true, SigLevel::USE_DEFAULT).is_ok(),
    }
}
