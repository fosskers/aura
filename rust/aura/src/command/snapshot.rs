//! All functionality involving the `-B` command.

use crate::error::Error;
use crate::{a, aln, aura, green, red};
use alpm::Alpm;
use aura_core::snapshot::Snapshot;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::fs::File;
use std::io::BufWriter;
use std::ops::Not;
use std::path::{Path, PathBuf};

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

pub(crate) fn restore(fll: &FluentLanguageLoader, cache: &Path) -> Result<(), Error> {
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
    println!("{}", index);

    green!(fll, "common-done");
    Ok(())
}
