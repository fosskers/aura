//! All functionality involving the `-B` command.

use crate::aura;
use crate::env::Env;
use crate::error::Nested;
use crate::green;
use crate::localization::Localised;
use crate::proceed;
use crate::utils::PathStr;
use crate::utils::NOTHING;
use aura_core::snapshot::Snapshot;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use r2d2_alpm::Alpm;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufWriter;
use std::ops::Not;
use std::path::Path;
use std::path::PathBuf;
use time::macros::format_description;

pub(crate) enum Error {
    Pacman(crate::pacman::Error),
    Readline(std::io::Error),
    JsonWrite(PathBuf, serde_json::Error),
    DeleteFile(PathBuf, std::io::Error),
    OpenFile(PathBuf, std::io::Error),
    TimeFormat(time::error::Format),
    Cancelled,
    NoSnapshots,
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Pacman(e) => e.nested(),
            Error::Readline(e) => error!("{e}"),
            Error::JsonWrite(_, e) => error!("{e}"),
            Error::DeleteFile(_, e) => error!("{e}"),
            Error::OpenFile(_, e) => error!("{e}"),
            Error::Cancelled => {}
            Error::NoSnapshots => {}
            Error::TimeFormat(e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Pacman(e) => e.localise(fll),
            Error::Readline(_) => fl!(fll, "err-user-input"),
            Error::JsonWrite(p, _) => fl!(fll, "err-json-write", file = p.utf8()),
            Error::Cancelled => fl!(fll, "common-cancelled"),
            Error::NoSnapshots => fl!(fll, "B-none"),
            Error::DeleteFile(p, _) => fl!(fll, "err-file-del", file = p.utf8()),
            Error::OpenFile(p, _) => fl!(fll, "err-file-open", file = p.utf8()),
            Error::TimeFormat(_) => fl!(fll, "err-time-format"),
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
    let form =
        format_description!("[year].[month]([month repr:short]).[day].[hour].[minute].[second]");
    let name = format!(
        "{}.json",
        snap.time.format(form).map_err(Error::TimeFormat)?
    );
    let path = snapshots.join(name);

    let file = BufWriter::new(File::create(&path).map_err(|e| Error::OpenFile(path.clone(), e))?);
    serde_json::to_writer(file, &snap).map_err(|e| Error::JsonWrite(path, e))?;
    green!(fll, "B-saved");

    Ok(())
}

/// Remove all saveds snapshots that don't have tarballs in the cache.
pub(crate) fn clean(fll: &FluentLanguageLoader, env: &Env) -> Result<(), Error> {
    proceed!(fll, env, "B-clean").ok_or(Error::Cancelled)?;
    let caches = env.caches();
    let snapshots = env.backups.snapshots.as_path();
    let vers = aura_core::cache::all_versions(&caches);

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
    let mut paths: Vec<_> = aura_core::snapshot::snapshots_with_paths(snapshots)
        .map(|(path, _)| path)
        .collect();
    paths.sort();

    for path in paths {
        println!("{}", path.display());
    }

    Ok(())
}

pub(crate) fn restore(env: &Env, fll: &FluentLanguageLoader, alpm: &Alpm) -> Result<(), Error> {
    let caches = env.caches();
    let snapshots = &env.backups.snapshots;
    let vers = aura_core::cache::all_versions(&caches);

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
        let form = format_description!("[year]-[month]-[day] [hour]-[minute]-[second]");
        let time = ss.time.format(form).map_err(Error::TimeFormat)?;
        let pinned = ss.pinned.then(|| "[pinned]".cyan()).unwrap_or_default();
        println!(" {:w$}) {} {}", i, time, pinned, w = digits);
    }

    let index = crate::utils::select(">>> ", shots.len() - 1).map_err(Error::Readline)?;
    restore_snapshot(env, alpm, &caches, shots.remove(index))?;

    green!(fll, "common-done");
    Ok(())
}

fn restore_snapshot(
    env: &Env,
    alpm: &Alpm,
    caches: &[&Path],
    snapshot: Snapshot,
) -> Result<(), Error> {
    let installed: HashMap<&str, &str> = alpm
        .as_ref()
        .localdb()
        .pkgs()
        .iter()
        .map(|p| (p.name(), p.version().as_str()))
        .collect();
    let diff = package_diff(&snapshot, &installed);

    // Alter packages first to avoid potential breakage from the later removal
    // step.
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

        crate::pacman::pacman_install_from_tarball(env, NOTHING, tarballs)
            .map_err(Error::Pacman)?;
    }

    // Remove packages that weren't installed within the chosen snapshot.
    if diff.to_remove.is_empty().not() {
        crate::pacman::sudo_pacman(env, "-R", NOTHING, diff.to_remove).map_err(Error::Pacman)?;
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
