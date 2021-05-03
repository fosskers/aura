//! Snapshot manipulation internals.

use alpm::Alpm;
use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::{File, ReadDir};
use std::io::BufReader;
use std::path::{Path, PathBuf};

/// An iterator of all legal (but not necessarily usable) [`Snapshot`]s.
pub struct Snapshots {
    with_paths: SnapshotsWithPaths,
}

impl Iterator for Snapshots {
    type Item = Snapshot;

    fn next(&mut self) -> Option<Self::Item> {
        self.with_paths.next().map(|(_, snap)| snap)
    }
}

/// All legal [`Snapshot`]s along with the full [`PathBuf`] they was read from.
pub struct SnapshotsWithPaths {
    read_dir: ReadDir,
}

impl Iterator for SnapshotsWithPaths {
    type Item = (PathBuf, Snapshot);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self
            .read_dir
            .next()?
            .ok()
            .and_then(|entry| {
                let p = entry.path();
                File::open(&p).ok().map(|f| (p, f))
            })
            .and_then(|(p, f)| {
                serde_json::from_reader(BufReader::new(f))
                    .ok()
                    .map(|s| (p, s))
            });

        match next {
            None => self.next(),
            n => n,
        }
    }
}

/// All packages installed at some specific [`DateTime`]. Any "pinned" snapshot
/// should never be considered for deletion.
#[derive(Serialize, Deserialize)]
pub struct Snapshot {
    /// The local date and time of when this snapshot was taken.
    pub time: DateTime<Local>,
    /// Should this `Snapshot` never be removed?
    pub pinned: bool,
    /// Every package name in the `Snapshot`, with its version.
    pub packages: HashMap<String, String>,
}

impl Snapshot {
    /// Given a handle to ALPM, take a snapshot of all currently installed
    /// packages and their versions.
    pub fn from_alpm(alpm: &Alpm) -> Snapshot {
        let time = Local::now();
        let packages = alpm
            .localdb()
            .pkgs()
            .iter()
            .map(|p| (p.name().to_owned(), p.version().as_str().to_owned()))
            .collect();

        Snapshot {
            time,
            pinned: false,
            packages,
        }
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
pub fn snapshots(snapshots_dir: &Path) -> Result<Snapshots, std::io::Error> {
    let with_paths = snapshots_with_paths(snapshots_dir)?;
    Ok(Snapshots { with_paths })
}

/// An iterator of all legal [`Snapshot`]s along with the full [`PathBuf`] they
/// was read from.
pub fn snapshots_with_paths(snapshots_dir: &Path) -> Result<SnapshotsWithPaths, std::io::Error> {
    let read_dir = snapshots_dir.read_dir()?;
    Ok(SnapshotsWithPaths { read_dir })
}
