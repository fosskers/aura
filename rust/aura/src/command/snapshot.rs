//! All functionality involving the `-B` command.

use crate::{a, aln, aura, green, red};
use crate::{error::Error, utils};
use alpm::Alpm;
use aura_core::snapshot::Snapshot;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufWriter;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::{cmp::Ordering, ffi::OsStr};

/// During a `-Br`, the packages to update and/or remove.
#[derive(Debug)]
struct StateDiff<'a> {
    /// Packages that need to be reinstalled, upgraded, or downgraded.
    to_add_or_alter: HashMap<&'a str, &'a str>,

    /// Packages that are installed now, but weren't when the snapshot was
    /// taken.
    to_remove: HashSet<&'a str>,
}

/// The full path to package snapshot directory.
pub fn snapshot_dir() -> Result<PathBuf, std::env::VarError> {
    let mut path = crate::utils::aura_xdg_cache()?;
    path.push("snapshots");
    Ok(path)
}

pub(crate) fn save(fll: &FluentLanguageLoader, alpm: &Alpm) -> Result<(), Error> {
    let mut cache = snapshot_dir()?;
    let snap = Snapshot::from_alpm(alpm);
    let name = format!("{}.json", snap.time.format("%Y.%m(%b).%d.%H.%M.%S"));
    cache.push(name);

    let file = BufWriter::new(File::create(cache)?);
    serde_json::to_writer(file, &snap)?;
    green!(fll, "B-saved");

    Ok(())
}

/// Remove all saveds snapshots that don't have tarballs in the cache.
pub(crate) fn clean(fll: &FluentLanguageLoader, cache: &Path) -> Result<(), Error> {
    let msg = format!("{} {} ", fl!(fll, "B-clean"), fl!(fll, "proceed-yes"));
    crate::utils::prompt(&a!(msg))?;

    let path = snapshot_dir()?;
    let vers = aura_core::cache::all_versions(cache)?;

    for (path, snapshot) in aura_core::snapshot::snapshots_with_paths(&path)? {
        if snapshot.pinned.not() && snapshot.usable(&vers).not() {
            std::fs::remove_file(path)?;
        }
    }

    green!(fll, "common-done");
    Ok(())
}

/// Show all saved package snapshot filenames.
pub(crate) fn list() -> Result<(), Error> {
    let snap = snapshot_dir()?;

    for (path, _) in aura_core::snapshot::snapshots_with_paths(&snap)? {
        println!("{}", path.display());
    }

    Ok(())
}

pub(crate) fn restore(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) -> Result<(), Error> {
    let snap = snapshot_dir()?;
    let vers = aura_core::cache::all_versions(cache)?;

    let mut shots: Vec<_> = aura_core::snapshot::snapshots(&snap)?
        .filter(|ss| ss.usable(&vers))
        .collect();
    shots.sort_by_key(|ss| ss.time);
    let digits = 1 + (shots.len() / 10);

    if shots.is_empty() {
        red!(fll, "B-none");
        return Err(Error::Silent);
    }

    aura!(fll, "B-select");
    for (i, ss) in shots.iter().enumerate() {
        let time = ss.time.format("%Y %B %d %T");
        let pinned = ss.pinned.then(|| "[pinned]".cyan()).unwrap_or_default();
        println!(" {:w$}) {} {}", i, time, pinned, w = digits);
    }

    let index = crate::utils::select(">>> ", shots.len() - 1)?;
    restore_snapshot(alpm, cache, shots.remove(index))?;

    green!(fll, "common-done");
    Ok(())
}

fn restore_snapshot(alpm: &Alpm, cache: &Path, snapshot: Snapshot) -> Result<(), Error> {
    let installed: HashMap<&str, &str> = alpm
        .localdb()
        .pkgs()
        .iter()
        .map(|p| (p.name(), p.version().as_str()))
        .collect();
    let diff = package_diff(&snapshot, &installed);

    // Alter packages first to avoid potential breakage from the later removal
    // step.
    if diff.to_add_or_alter.is_empty().not() {
        let tarballs = aura_core::cache::package_paths(cache)?
            .filter(|pp| {
                let p = pp.to_package();
                match diff.to_add_or_alter.get(p.name.as_str()) {
                    Some(v) if p.same_version(v) => true,
                    Some(_) | None => false,
                }
            })
            .map(|pp| pp.pathbuf().into_os_string());

        utils::sudo_pacman(std::iter::once(OsStr::new("-U").to_os_string()).chain(tarballs))?;
    }

    // Remove packages that weren't installed within the chosen snapshot.
    if diff.to_remove.is_empty().not() {
        utils::sudo_pacman(std::iter::once("-R").chain(diff.to_remove))?;
    }

    Ok(())
}

// TODO Audit the lifetimes.
fn package_diff<'a>(
    snapshot: &'a Snapshot,
    installed: &'a HashMap<&'a str, &'a str>,
) -> StateDiff<'a> {
    let mut to_add_or_alter: HashMap<&'a str, &'a str> = HashMap::new();
    let mut to_remove: HashSet<&'a str> = HashSet::new();

    for (name, ver) in snapshot.packages.iter() {
        // If a package saved in the snapshot isn't installed at all anymore, it
        // needs to be reinstalled.
        if installed.contains_key(name.as_str()).not() {
            to_add_or_alter.insert(name, ver);
        }
    }

    for (name, ver) in installed.iter() {
        match snapshot.packages.get(*name) {
            // If an installed package wasn't in the snapshot at all, we need to
            // remove it.
            None => {
                to_remove.insert(name);
            }
            Some(v) => match alpm::vercmp(v.as_str(), ver) {
                // The installed version is the same as the snapshot; no action
                // necessary.
                Ordering::Equal => {}
                // Otherwise, the version in the snapshot must be installed.
                Ordering::Less | Ordering::Greater => {
                    to_add_or_alter.insert(name, v);
                }
            },
        }
    }

    StateDiff {
        to_add_or_alter,
        to_remove,
    }
}
