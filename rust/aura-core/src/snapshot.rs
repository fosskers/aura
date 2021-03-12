//! Snapshot manipulation internals.

use alpm::Alpm;
use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// All packages installed at some specific [`DateTime`]. Any "pinned" snapshot
/// should never be considered for deletion.
#[derive(Serialize, Deserialize)]
pub struct Snapshot {
    time: DateTime<Local>,
    pinned: bool,
    packages: HashMap<String, String>,
}

impl Snapshot {
    /// Does this `Snapshot` match what is currently installed?
    pub fn current(&self, alpm: &Alpm) -> bool {
        alpm.localdb()
            .pkgs()
            .iter()
            .all(|p| self.packages.contains_key(p.name()))
    }

    /// Do tarballs exist in the package cache for every package in this `Snapshot`?
    pub fn usable(&self, tarball_cache: &Path) -> bool {
        match crate::cache::all_versions(tarball_cache) {
            Err(_) => false,
            Ok(vs) => self
                .packages
                .iter()
                .all(|(k, v)| vs.get(k).map(|set| set.contains(v)).unwrap_or(false)),
        }
    }
}
