//! Snapshot manipulation internals.

use alpm::Alpm;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use time::OffsetDateTime;

/// All packages installed at some specific [`DateTime`]. Any "pinned" snapshot
/// should never be considered for deletion.
#[derive(Serialize, Deserialize)]
pub struct Snapshot {
    /// The local date and time of when this snapshot was taken.
    pub time: OffsetDateTime,
    /// Should this `Snapshot` never be removed?
    pub pinned: bool,
    /// Every package name in the `Snapshot`, with its version.
    pub packages: HashMap<String, String>,
}

impl Snapshot {
    /// Given a handle to ALPM, take a snapshot of all currently installed
    /// packages and their versions.
    pub fn from_alpm(alpm: &Alpm) -> Result<Snapshot, time::error::IndeterminateOffset> {
        let time = OffsetDateTime::now_local()?;
        let packages = alpm
            .localdb()
            .pkgs()
            .iter()
            .map(|p| (p.name().to_owned(), p.version().as_str().to_owned()))
            .collect();

        let snap = Snapshot {
            time,
            pinned: false,
            packages,
        };

        Ok(snap)
    }

    /// Does this `Snapshot` match what is currently installed?
    pub fn current(&self, alpm: &Alpm) -> bool {
        alpm.localdb()
            .pkgs()
            .iter()
            .all(|p| self.packages.contains_key(p.name()))
    }

    /// Do tarballs exist in the package cache for every package in this `Snapshot`?
    ///
    /// Accepts a `HashMap` assumed to have come from [`crate::cache::all_versions`].
    pub fn usable(&self, versions: &HashMap<String, HashSet<String>>) -> bool {
        self.packages
            .iter()
            .all(|(k, v)| versions.get(k).map(|set| set.contains(v)).unwrap_or(false))
    }
}

/// An iterator of all legal [`Snapshot`]s.
pub fn snapshots(snapshots_d: &Path) -> impl Iterator<Item = Snapshot> {
    snapshots_with_paths(snapshots_d).map(|(_, s)| s)
}

/// An iterator of all legal [`Snapshot`]s along with the full [`PathBuf`] they
/// were read from.
pub fn snapshots_with_paths(snapshots_d: &Path) -> impl Iterator<Item = (PathBuf, Snapshot)> {
    snapshots_d
        .read_dir()
        .into_iter()
        .flatten()
        .filter_map(|r| r.ok())
        .filter_map(|entry| {
            let p = entry.path();
            File::open(&p).ok().map(|f| (p, f))
        })
        .filter_map(|(p, f)| {
            serde_json::from_reader(BufReader::new(f))
                .ok()
                .map(|s| (p, s))
        })
}
