//! All functionality involving the `-C` command.

use crate::error::Error;
use crate::{a, aln};
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::debug;
use pbr::ProgressBar;
use rayon::prelude::*;
use rustyline::Editor;
use std::path::Path;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use ubyte::ToByteUnit;

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
    // The full, absolute path to copy files to.
    let full: PathBuf = if target.is_absolute() {
        target.to_path_buf()
    } else {
        let mut curr = std::env::current_dir().map_err(Error::IO)?;
        curr.push(target);
        curr
    };
    let ts = full.to_str().unwrap();
    if target.is_file() {
        let msg = fl!(fll, "cache-backup-file", target = ts);
        aln!(msg.red());
        Err(Error::Silent)
    } else {
        // How big is the current cache?
        let (file_count, cache_bytes): (u64, u64) = source
            .read_dir()
            .map_err(Error::IO)?
            .filter_map(|de| de.ok())
            .filter_map(|de| de.metadata().ok())
            .map(|meta| meta.len())
            .fold((0, 0), |(ac, al), l| (ac + 1, al + l));
        let size = format!("{}", cache_bytes.bytes());
        aln!(fl!(fll, "cache-backup-size", size = size));

        // Is the target directory empty?
        let target_count = target.read_dir().map(|d| d.count()).unwrap_or(0);
        if target_count > 0 {
            aln!(fl!(fll, "cache-backup-nonempty", target = ts).yellow());
        } else {
            aln!(fl!(fll, "cache-backup-target", target = ts));
        }
        let mut rl = Editor::<()>::new();
        let msg = format!("{} {} ", fl!(fll, "proceed"), fl!(fll, "proceed-yes"));
        match rl.readline(&a!(msg)) {
            Ok(line) if line.is_empty() || line == "y" || line == "Y" => {
                copy(source, &full, file_count)
            }
            Ok(_) => Err(Error::Rejected),
            Err(e) => Err(Error::RustyLine(e)),
        }
    }
}

/// Copy all the cache files concurrently.
fn copy(source: &Path, target: &Path, file_count: u64) -> Result<(), Error> {
    debug!("Begin cache copying.");

    // A progress bar to display the copying progress.
    let pb = Arc::new(Mutex::new(ProgressBar::new(file_count)));

    // Silently succeeds if the directory already exists.
    std::fs::create_dir_all(target).map_err(Error::IO)?;

    source
        .read_dir()
        .map_err(Error::IO)?
        .filter_map(|entry| entry.ok())
        .filter_map(|entry| {
            let from = entry.path();
            entry.path().file_name().map(|name| {
                let mut to = target.to_path_buf();
                to.push(name);
                (from, to)
            })
        })
        .par_bridge()
        .for_each(|(from, to)| {
            if let Ok(_) = std::fs::copy(from, to) {
                pb.lock().unwrap().inc();
            }
        });
    Ok(())
}
