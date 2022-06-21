//! All functionality involving the `-B` command.

use crate::localization::Localised;
use crate::utils::PathStr;
use crate::{aura, green, proceed};
use alpm::Alpm;
use aura_core::snapshot::Snapshot;
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufWriter;
use std::ops::Not;
use std::path::{Path, PathBuf};

#[derive(FromVariants)]
pub(crate) enum Error {
    Dirs(crate::dirs::Error),
    Pacman(crate::pacman::Error),
    Readline(rustyline::error::ReadlineError),
    #[from_variants(skip)]
    JsonWrite(PathBuf, serde_json::Error),
    #[from_variants(skip)]
    DeleteFile(PathBuf, std::io::Error),
    #[from_variants(skip)]
    OpenFile(PathBuf, std::io::Error),
    Cancelled,
    NoSnapshots,
}

impl Error {
    pub(crate) fn nested(&self) {
        match self {
            Error::Dirs(e) => e.nested(),
            Error::Pacman(e) => e.nested(),
            Error::Readline(e) => error!("{e}"),
            Error::JsonWrite(_, e) => error!("{e}"),
            Error::DeleteFile(_, e) => error!("{e}"),
            Error::OpenFile(_, e) => error!("{e}"),
            Error::Cancelled => {}
            Error::NoSnapshots => {}
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Dirs(e) => e.localise(fll),
            Error::Pacman(e) => e.localise(fll),
            Error::Readline(_) => fl!(fll, "err-user-input"),
            Error::JsonWrite(p, _) => fl!(fll, "err-json-write", file = p.utf8()),
            Error::Cancelled => fl!(fll, "common-cancelled"),
            Error::NoSnapshots => fl!(fll, "B-none"),
            Error::DeleteFile(p, _) => fl!(fll, "err-file-del", file = p.utf8()),
            Error::OpenFile(p, _) => fl!(fll, "err-file-open", file = p.utf8()),
        }
    }
}

/// During a `-Br`, the packages to update and/or remove.
#[derive(Debug)]
struct StateDiff<'a> {
    /// Packages that need to be reinstalled, upgraded, or downgraded.
    to_add_or_alter: HashMap<&'a str, &'a str>,

    /// Packages that are installed now, but weren't when the snapshot was
    /// taken.
    to_remove: HashSet<&'a str>,
}

pub(crate) fn save(fll: &FluentLanguageLoader, alpm: &Alpm, snapshots: &Path) -> Result<(), Error> {
    let snap = Snapshot::from_alpm(alpm);
    let name = format!("{}.json", snap.time.format("%Y.%m(%b).%d.%H.%M.%S"));
    let path = snapshots.join(name);

    let file = BufWriter::new(File::create(&path).map_err(|e| Error::OpenFile(path.clone(), e))?);
    serde_json::to_writer(file, &snap).map_err(|e| Error::JsonWrite(path, e))?;
    green!(fll, "B-saved");

    Ok(())
}

/// Remove all saveds snapshots that don't have tarballs in the cache.
pub(crate) fn clean(
    fll: &FluentLanguageLoader,
    caches: &[&Path],
    snapshots: &Path,
) -> Result<(), Error> {
    proceed!(fll, "B-clean").ok_or(Error::Cancelled)?;
    let vers = aura_core::cache::all_versions(caches);

    for (path, snapshot) in aura_core::snapshot::snapshots_with_paths(snapshots) {
        if snapshot.pinned.not() && snapshot.usable(&vers).not() {
            std::fs::remove_file(&path).map_err(|e| Error::DeleteFile(path, e))?;
        }
    }

    green!(fll, "common-done");
    Ok(())
}

/// Show all saved package snapshot filenames.
pub(crate) fn list(snapshots: &Path) -> Result<(), Error> {
    for (path, _) in aura_core::snapshot::snapshots_with_paths(snapshots) {
        println!("{}", path.display());
    }

    Ok(())
}

pub(crate) fn restore(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    caches: &[&Path],
    snapshots: &Path,
) -> Result<(), Error> {
    let vers = aura_core::cache::all_versions(caches);

    let mut shots: Vec<_> = aura_core::snapshot::snapshots(snapshots)
        .filter(|ss| ss.usable(&vers))
        .collect();
    shots.sort_by_key(|ss| ss.time);
    let digits = 1 + (shots.len() / 10);

    if shots.is_empty() {
        return Err(Error::NoSnapshots);
    }

    aura!(fll, "B-select");
    for (i, ss) in shots.iter().enumerate() {
        let time = ss.time.format("%Y %B %d %T");
        let pinned = ss.pinned.then(|| "[pinned]".cyan()).unwrap_or_default();
        println!(" {:w$}) {} {}", i, time, pinned, w = digits);
    }

    let index = crate::utils::select(">>> ", shots.len() - 1)?;
    restore_snapshot(alpm, caches, shots.remove(index))?;

    green!(fll, "common-done");
    Ok(())
}

fn restore_snapshot(alpm: &Alpm, caches: &[&Path], snapshot: Snapshot) -> Result<(), Error> {
    let installed: HashMap<&str, &str> = alpm
        .localdb()
        .pkgs()
        .iter()
        .map(|p| (p.name(), p.version().as_str()))
        .collect();
    let diff = package_diff(&snapshot, &installed);

    // Alter packages first to avoid potential breakage from the later removal
    // step.
    let nothing: [&str; 0] = [];
    if diff.to_add_or_alter.is_empty().not() {
        let tarballs = aura_core::cache::package_paths(caches)
            .filter(|pp| {
                let p = pp.as_package();
                match diff.to_add_or_alter.get(p.name.as_ref()) {
                    Some(v) if p.same_version(v) => true,
                    Some(_) | None => false,
                }
            })
            .map(|pp| pp.into_pathbuf().into_os_string());

        crate::pacman::sudo_pacman("-U", nothing, tarballs)?;
    }

    // Remove packages that weren't installed within the chosen snapshot.
    if diff.to_remove.is_empty().not() {
        crate::pacman::sudo_pacman("-R", nothing, diff.to_remove)?;
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
