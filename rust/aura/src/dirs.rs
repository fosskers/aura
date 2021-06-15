//! Directories critical to Aura's function.

use std::path::PathBuf;

/// The full path to package snapshot directory.
pub fn snapshot() -> Result<PathBuf, std::env::VarError> {
    let mut path = crate::utils::aura_xdg_cache()?;
    path.push("snapshots");
    Ok(path)
}
