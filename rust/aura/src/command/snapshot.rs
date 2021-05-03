//! All functionality involving the `-B` command.

use crate::error::Error;
use crate::{a, aln, aura, green, red};
use alpm::Alpm;
use aura_core::snapshot::Snapshot;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufWriter;
use std::ops::Not;
use std::path::{Path, PathBuf};

/// During a `-Br`, the packages to update and/or remove.
struct StateDiff<'a> {
    /// Packages that need to be reinstalled.
    to_add: Vec<&'a str>,

    /// Packages that need to be upgraded or downgraded.
    to_alter: Vec<&'a str>,

    /// Packages that are installed now, but weren't when the snapshot was
    /// taken.
    to_remove: Vec<&'a str>,
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
    // TODO Consider using the raw ISO representation, so that selection in -Br
    // can be prettier. Namely, a true `DateTime` can be pulled back out of the
    // name, and then pretty-printed in a human-friendly way on the prompt.
    //
    // No! Just use `NaiveDateTime::parse_from_str`!
    let name = format!("{}.json", snap.time.format("%Y.%m(%b).%d.%H.%M.%S"));
    cache.push(name);

    let file = BufWriter::new(File::create(cache)?);
    serde_json::to_writer(file, &snap)?;
    green!(fll, "snapshot-saved");

    Ok(())
}

/// Remove all saveds snapshots that don't have tarballs in the cache.
pub(crate) fn clean(fll: &FluentLanguageLoader, cache: &Path) -> Result<(), Error> {
    let msg = format!(
        "{} {} ",
        fl!(fll, "snapshot-clean"),
        fl!(fll, "proceed-yes")
    );
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
        red!(fll, "snapshot-none");
        Err(Error::Silent)?
    }

    aura!(fll, "snapshot-select");
    for (i, ss) in shots.iter().enumerate() {
        let time = ss.time.format("%Y %B %d %T");
        let pinned = ss
            .pinned
            .then(|| "[pinned]".cyan())
            .unwrap_or_else(|| "".normal());
        println!(" {:w$}) {} {}", i, time, pinned, w = digits);
    }

    let index = crate::utils::select(">>> ", shots.len() - 1)?;
    restore_snapshot(alpm, shots.remove(index))?;

    green!(fll, "common-done");
    Ok(())
}

// TODO Find the diff
fn restore_snapshot(alpm: &Alpm, snapshot: Snapshot) -> Result<(), Error> {
    let diff = package_diff(alpm, &snapshot)?;

    Ok(())
}

fn package_diff<'a>(alpm: &'a Alpm, snapshot: &'a Snapshot) -> Result<StateDiff<'a>, Error> {
    let mut to_add = Vec::new();
    let mut to_alter = Vec::new();
    let mut to_remove = Vec::new();

    let installed: HashMap<&'a str, &'a str> = alpm
        .localdb()
        .pkgs()
        .iter()
        .map(|p| (p.name(), p.version().as_str()))
        .collect();

    for name in snapshot.packages.keys() {
        // If a package saved in the snapshot isn't installed at all anymore, it
        // needs to be reinstalled.
        if installed.contains_key(name.as_str()).not() {
            to_add.push(name.as_str());
        }
    }

    for (name, ver) in installed.into_iter() {
        // If an installed package wasn't in the snapshot at all, we need to
        // remove it.
        if snapshot.packages.contains_key(name).not() {
            to_remove.push(name);
        }
    }

    let diff = StateDiff {
        to_add,
        to_alter,
        to_remove,
    };
    Ok(diff)
}
