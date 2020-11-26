//! All functionality involving the `-C` command.

use crate::aln;
use crate::error::Error;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::path::Path;

/// Print all package filepaths from the cache that match some search term.
pub fn search(path: &Path, term: String) -> Result<(), Error> {
    let matches = aura_core::cache::search(path, term).map_err(Error::IO)?;
    for file in matches {
        println!("{}", file.display());
    }
    Ok(())
}

/// Backup the package cache to a given directory.
pub fn backup(fll: FluentLanguageLoader, source: &Path, target: &Path) -> Result<(), Error> {
    if target.is_file() {
        let ts = target.to_str().unwrap();
        let msg = fl!(fll, "cache-backup-target", target = ts).red();
        aln!(msg);
        Err(Error::Silent)
    } else {
        Ok(())
    }
    // target.read_dir()
    // std::fs::create_dir_all(target).map_err(Error::IO)?;
    // Ok(())
}
