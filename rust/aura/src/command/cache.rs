//! All functionality involving the `-C` command.

use crate::{a, aln, error};
use anyhow::{Context, Result};
use aura_core as core;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::debug;
use pbr::ProgressBar;
use rayon::prelude::*;
use std::path::Path;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use ubyte::ToByteUnit;

/// Print all package filepaths from the cache that match some search term.
pub fn search(path: &Path, term: String) -> Result<()> {
    let matches = core::cache::search(path, term)?;
    for file in matches {
        println!("{}", file.display());
    }
    Ok(())
}

/// Backup the package cache to a given directory.
pub fn backup(fll: FluentLanguageLoader, source: &Path, target: &Path) -> Result<()> {
    // The full, absolute path to copy files to.
    let full: PathBuf = if target.is_absolute() {
        target.to_path_buf()
    } else {
        let mut curr = std::env::current_dir().context("failed to get current directory")?;
        curr.push(target);
        curr
    };
    let ts = full.to_str().unwrap();
    if target.is_file() {
        let msg = fl!(fll, "cache-backup-file", target = ts);
        aln!(msg.red());
        error::silent()
    } else {
        // How big is the current cache?
        let cache_size: core::cache::CacheSize = core::cache::size(source)?;
        let size = format!("{}", cache_size.bytes.bytes());
        aln!(fl!(fll, "cache-backup-size", size = size));

        // Is the target directory empty?
        let target_count = target.read_dir().map(|d| d.count()).unwrap_or(0);
        if target_count > 0 {
            aln!(fl!(fll, "cache-backup-nonempty", target = ts).yellow());
        } else {
            aln!(fl!(fll, "cache-backup-target", target = ts));
        }

        // Proceed if the user accepts.
        let msg = format!("{} {} ", fl!(fll, "proceed"), fl!(fll, "proceed-yes"));
        crate::utils::prompt(&a!(msg))?;
        copy(source, &full, cache_size.files)
    }
}

/// Copy all the cache files concurrently.
fn copy(source: &Path, target: &Path, file_count: u64) -> Result<()> {
    debug!("Begin cache copying.");

    // TODO Change the bar style.
    // A progress bar to display the copying progress.
    let pb = Arc::new(Mutex::new(ProgressBar::new(file_count)));

    // Silently succeeds if the directory already exists.
    std::fs::create_dir_all(target)
        .with_context(|| format!("failed to mkdir '{}'", target.display()))?;

    source
        .read_dir()
        .with_context(|| format!("failed to read '{}'", source.display()))?
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
            if std::fs::copy(from, to).is_ok() {
                pb.lock().unwrap().inc();
            }
        });
    Ok(())
}
