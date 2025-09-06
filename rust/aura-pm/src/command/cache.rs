//! All functionality involving the `-C` command.

use crate::aura;
use crate::env::Env;
use crate::error::Nested;
use crate::green;
use crate::localization::Localised;
use crate::proceed;
use crate::utils::PathStr;
use crate::utils::NOTHING;
use crate::yellow;
use aura_core::cache::CacheSize;
use aura_core::cache::PkgPath;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use itertools::Itertools;
use linya::Progress;
use log::debug;
use log::error;
use r2d2_alpm::Alpm;
use rayon::prelude::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ffi::OsString;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use time::macros::format_description;
use time::OffsetDateTime;
use ubyte::ToByteUnit;

pub(crate) enum Error {
    Readline(std::io::Error),
    Pacman(crate::pacman::Error),
    Cancelled,
    NoPackages,
    NothingToDo,
    AlreadyExists(PathBuf),
    Delete(PathBuf),
    ReadDir(PathBuf),
    Stdout(std::io::Error),
    CurrDir(std::io::Error),
    Mkdir(PathBuf, std::io::Error),
    Date(time::error::Format),
    Env(crate::env::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Readline(e) => error!("{e}"),
            Error::Pacman(e) => e.nested(),
            Error::Cancelled => {}
            Error::NoPackages => {}
            Error::NothingToDo => {}
            Error::AlreadyExists(_) => {}
            Error::Delete(_) => {}
            Error::ReadDir(_) => {}
            Error::Stdout(e) => error!("{e}"),
            Error::CurrDir(e) => error!("{e}"),
            Error::Mkdir(_, e) => error!("{e}"),
            Error::Date(e) => error!("{e}"),
            Error::Env(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Readline(_) => fl!(fll, "err-user-input"),
            Error::Pacman(e) => e.localise(fll),
            Error::Cancelled => fl!(fll, "common-cancelled"),
            Error::NoPackages => fl!(fll, "common-no-packages"),
            Error::NothingToDo => fl!(fll, "common-no-work"),
            Error::AlreadyExists(p) => fl!(fll, "C-b-file", target = p.utf8()),
            Error::Delete(p) => fl!(fll, "err-file-del", file = p.utf8()),
            Error::ReadDir(p) => fl!(fll, "err-read-dir", dir = p.utf8()),
            Error::Stdout(_) => fl!(fll, "err-write"),
            Error::CurrDir(_) => fl!(fll, "C-b-curr"),
            Error::Mkdir(p, _) => fl!(fll, "dir-mkdir", dir = p.utf8()),
            Error::Date(_) => fl!(fll, "err-time-format"),
            Error::Env(_) => fl!(fll, "err-alpm"),
        }
    }
}

/// Downgrade the given packages.
pub(crate) fn downgrade(
    env: &Env,
    fll: &FluentLanguageLoader,
    packages: Vec<String>,
) -> Result<(), Error> {
    // Exit early if the user passed no packages.
    if packages.is_empty() {
        return Err(Error::NoPackages);
    }

    let caches = env.caches();

    // --- All tarball paths for packages the user asked for --- //
    let mut tarballs: HashMap<&str, Vec<PkgPath>> = HashMap::new();
    for pp in aura_core::cache::package_paths(&caches) {
        if let Some(p) = packages.iter().find(|p| p == &&pp.as_package().name) {
            let paths = tarballs.entry(p).or_default();
            paths.push(pp);
        }
    }

    let to_downgrade: Vec<OsString> = packages
        .iter()
        .filter_map(|p| tarballs.remove(p.as_str()).map(|pps| (p, pps)))
        .filter_map(|(p, pps)| downgrade_one(fll, p, pps).ok())
        .map(|pp| pp.into_pathbuf().into_os_string())
        .collect();

    if to_downgrade.is_empty() {
        return Err(Error::NothingToDo);
    }

    crate::pacman::pacman_install_from_tarball(env, NOTHING, to_downgrade)
        .map_err(Error::Pacman)?;
    green!(fll, "common-done");
    Ok(())
}

fn downgrade_one(
    fll: &FluentLanguageLoader,
    package: &str,
    mut tarballs: Vec<PkgPath>,
) -> Result<PkgPath, Error> {
    tarballs.sort_by(|a, b| b.as_package().cmp(a.as_package()));
    let digits = 1 + (tarballs.len() / 10);

    let pkg = package.bold().cyan().to_string();
    aura!(fll, "C-downgrade-which", pkg = pkg);

    for (i, pp) in tarballs.iter().enumerate() {
        println!(" {:w$}) {}", i, pp.as_package().version, w = digits);
    }

    let index = crate::utils::select(">>> ", tarballs.len() - 1).map_err(Error::Readline)?;

    Ok(tarballs.remove(index))
}

/// Delete invalid tarballs from the cache.
pub(crate) fn invalid(
    env: &Env,
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    caches: &[&Path],
) -> Result<(), Error> {
    aura!(fll, "C-t-invalids");

    let elevation = env.sudo();

    // FIXME Thu Jan 27 2022 Use `Validated` here.
    aura_core::cache::package_paths(caches)
        .filter(|pp| !aura_core::is_valid_package(alpm, pp.as_path()))
        .for_each(|pp| {
            let _ = pp.sudo_remove_with_sig(elevation); // TODO Better handling.
        });

    green!(fll, "common-done");
    Ok(())
}

/// Print the contents of the package caches.
pub(crate) fn list(caches: &[&Path]) -> Result<(), Error> {
    let rds = caches
        .iter()
        .map(|path| {
            path.read_dir()
                .map_err(|_| Error::ReadDir(path.to_path_buf()))
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let mut paths: Vec<_> = rds
        .into_iter()
        .flat_map(|rd| rd.filter_map(|de| de.ok()).map(|de| de.path()))
        .collect();
    paths.sort();

    for path in paths {
        println!("{}", path.display());
    }

    Ok(())
}

/// Print cache data for given packages.
pub(crate) fn info(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    caches: &[&Path],
    packages: Vec<String>,
) -> Result<(), Error> {
    let db = alpm.as_ref().localdb();
    let mut w = BufWriter::new(std::io::stdout());

    let name = fl!(fll, "common-name");
    let ver = fl!(fll, "C-i-latest");
    let created = fl!(fll, "C-i-created");
    let sig = fl!(fll, "C-i-sig");
    let size = fl!(fll, "C-i-size");
    let av = fl!(fll, "C-i-avail");
    let inst = fl!(fll, "C-i-installed");

    let fmt = format_description!("[year]-[month]-[day] [hour]-[minute]-[second]");

    for ci in packages
        .iter()
        .filter_map(|p| aura_core::cache::info(caches, p).ok())
        .flatten()
    {
        let dt = OffsetDateTime::from(ci.created)
            .format(&fmt)
            .map_err(Error::Date)?;
        let sig_yes_no = if ci.signature {
            fl!(fll, "common-yes").green().bold()
        } else {
            fl!(fll, "common-no").yellow()
        };
        let is_in = if let Ok(pkg) = db.pkg(ci.name.as_str()) {
            if ci.version == pkg.version().as_str() {
                format!("[{inst}]").cyan().bold()
            } else {
                format!("[{}: {}]", inst, pkg.version()).yellow().bold()
            }
        } else {
            "".normal()
        };

        let pairs: Vec<(&str, ColoredString)> = vec![
            (&name, ci.name.normal()),
            (&ver, format!("{} {}", ci.version.normal(), is_in).normal()),
            (&created, dt.to_string().normal()),
            (&sig, sig_yes_no),
            (&size, format!("{}", ci.size.bytes()).normal()),
            (&av, ci.available.join(", ").normal()),
        ];

        crate::utils::info(&mut w, fll.current_language(), &pairs).map_err(Error::Stdout)?;
        writeln!(w).map_err(Error::Stdout)?;
    }

    Ok(())
}

/// Print all package filepaths from the cache that match some search term.
pub(crate) fn search(caches: &[&Path], term: &str) -> Result<(), Error> {
    for file in aura_core::cache::search(caches, term) {
        println!("{}", file.display());
    }
    Ok(())
}

/// Delete all but `keep`-many old tarballs for each package in the cache.
pub(crate) fn clean(
    env: &Env,
    fll: &FluentLanguageLoader,
    keep: usize,
    uninstalled: bool,
) -> Result<(), Error> {
    let caches = env.caches();
    debug!("Caches: {:?}", caches);

    let size_before = aura_core::cache::size(&caches);
    let human = format!("{}", size_before.bytes.bytes());
    aura!(fll, "C-size", size = human);
    yellow!(fll, "C-c-keep", pkgs = keep);

    // Proceed if the user accepts.
    proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;

    let alpm = env.alpm().map_err(Error::Env)?;
    let db = alpm.as_ref().localdb();
    let elevation = env.sudo();

    // Get all the tarball paths, sort and group them by name, and then remove them.
    aura_core::cache::package_paths(&caches)
        .sorted_by(|p0, p1| p1.cmp(p0)) // Forces a `collect` underneath.
        .chunk_by(|pp| pp.as_package().name.clone()) // TODO Naughty clone.
        .into_iter()
        .filter(|(name, _)| !uninstalled || db.pkg(name.as_ref()).is_err())
        .flat_map(|(_, group)| group.skip(keep)) // Thanks to the reverse-sort above, `group` is already backwards.
        .for_each(|pp| {
            let _ = pp.sudo_remove_with_sig(elevation); // TODO Handle this error better?
        });

    let size_after = aura_core::cache::size(&caches);
    let freed = format!("{}", (size_before.bytes - size_after.bytes).bytes());
    green!(fll, "C-c-freed", bytes = freed);
    Ok(())
}

/// Delete only those tarballs which aren't present in a snapshot.
pub(crate) fn clean_not_saved(fll: &FluentLanguageLoader, env: &Env) -> Result<(), Error> {
    let caches = env.caches();

    // Report the initial size of the cache.
    let size_before = aura_core::cache::size(&caches);
    let human = format!("{}", size_before.bytes.bytes());
    aura!(fll, "C-size", size = human);

    // Proceed if the user accepts.
    proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;

    let tarballs = aura_core::cache::package_paths(&caches);

    // Every package across all snapshots, keyed to all unique versions present.
    let snaps: HashMap<String, HashSet<String>> = {
        let mut snaps: HashMap<_, HashSet<_>> = HashMap::new();

        for snap in aura_core::snapshot::snapshots(&env.backups.snapshots) {
            for (name, ver) in snap.packages.into_iter() {
                let entry = snaps.entry(name).or_default();
                entry.insert(ver);
            }
        }

        snaps
    };

    let elevation = env.sudo();

    for tarball in tarballs {
        let p = tarball.as_package();

        // If no snapshot contains this tarball's particular version, remove it
        // from the filesystem.
        match snaps.get(p.name.as_ref()) {
            Some(vs) if vs.contains(&p.version.to_string()) => {}
            Some(_) | None => tarball.sudo_remove(elevation).map_err(Error::Delete)?,
        }
    }

    // Report the amount of disk space freed.
    let size_after = aura_core::cache::size(&caches);
    let freed = format!("{}", (size_before.bytes - size_after.bytes).bytes());
    green!(fll, "C-c-freed", bytes = freed);

    Ok(())
}

/// Download tarballs of installed packages that are missing from the cache.
pub(crate) fn refresh(env: &Env, fll: &FluentLanguageLoader, alpm: &Alpm) -> Result<(), Error> {
    let caches = env.caches();

    // All installed packages that are missing a tarball in the cache.
    let ps: Vec<&alpm::Package> = {
        let mut ps: Vec<_> = aura_core::cache::officials_missing_tarballs(alpm, &caches).collect();
        ps.sort_by(|a, b| a.name().cmp(b.name()));
        ps
    };

    if ps.is_empty() {
        green!(fll, "C-y-no-work");
    } else {
        crate::pacman::sudo_pacman(env, "-S", ["-w"], ps.iter().map(|p| p.name()))
            .map_err(Error::Pacman)?;
    }

    Ok(())
}

/// Backup your package caches to a given directory.
pub(crate) fn backup(fll: &FluentLanguageLoader, env: &Env, target: &Path) -> Result<(), Error> {
    let sources = env.caches();

    // The full, absolute path to copy files to.
    let full: PathBuf = if target.is_absolute() {
        target.to_path_buf()
    } else {
        std::env::current_dir()
            .map_err(Error::CurrDir)?
            .join(target)
    };
    let ts = full.to_str().unwrap();

    // Exit early if the target is an existing file, not a directory.
    if target.is_file() {
        return Err(Error::AlreadyExists(target.to_path_buf()));
    }

    // How big is the current cache?
    let cache_size: CacheSize = aura_core::cache::size(&sources);
    let size = format!("{}", cache_size.bytes.bytes());
    aura!(fll, "C-size", size = size);

    // Is the target directory empty?
    let target_count = target.read_dir().map(|d| d.count()).unwrap_or(0);
    if target_count > 0 {
        yellow!(fll, "C-b-nonempty", target = ts);
    } else {
        aura!(fll, "C-b-target", target = ts);
    }

    // Proceed if the user accepts.
    proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;

    copy(&sources, &full, cache_size.files)
}

/// Copy all the cache files concurrently.
fn copy(sources: &[&Path], target: &Path, file_count: usize) -> Result<(), Error> {
    debug!("Begin cache copying.");

    // TODO Change the bar style.
    // A progress bar to display the copying progress.
    let pb = Arc::new(Mutex::new(Progress::new()));
    let bar = pb.lock().unwrap().bar(file_count, "File Copying"); // TODO Localize.

    // Silently succeeds if the directory already exists.
    std::fs::create_dir_all(target).map_err(|e| Error::Mkdir(target.to_path_buf(), e))?;

    aura_core::read_dirs(sources)
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
        .for_each_with(pb, |p, (from, to)| {
            if std::fs::copy(from, to).is_ok() {
                p.lock().unwrap().inc_and_draw(&bar, 1);
            }
        });
    Ok(())
}

/// Display packages that don't have a tarball in any cache.
pub(crate) fn missing(alpm: &Alpm, caches: &[&Path]) {
    for pkg in aura_core::cache::missing_tarballs(alpm, caches) {
        println!("{} {}", pkg.name(), pkg.version());
    }
}
