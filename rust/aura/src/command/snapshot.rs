//! All functionality involving the `-B` command.

use crate::error::Error;
use crate::{aln, green};
use alpm::Alpm;
use aura_core::snapshot::Snapshot;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;

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
    green!(fll, "snapshot-saved");

    Ok(())
}
