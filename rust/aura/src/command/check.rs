//! Analyze many aspects of your installation for validity.

use alpm::Alpm;
use aura_core::cache::PkgPath;
use std::path::Path;

/// Is every tarball in the cache valid and loadable by ALPM?
///
/// Any that aren't are returned for reporting. Fails if the directory couldn't
/// be read.
fn valid_cache(alpm: &Alpm, cache: &Path) -> Result<Vec<PkgPath>, std::io::Error> {
    let bads = aura_core::cache::package_paths(cache)?
        .filter(|pp| !aura_arch::is_valid_package(alpm, pp.path()))
        .collect();

    Ok(bads)
}
