//! All functionality involving the `-C` command.

use crate::download::download_with_progress;
use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::PathStr;
use crate::{aura, green, proceed, yellow};
use alpm::Alpm;
use aura_core::cache::{CacheSize, PkgPath};
use colored::*;
use from_variants::FromVariants;
use i18n_embed::{fluent::FluentLanguageLoader, LanguageLoader};
use i18n_embed_fl::fl;
use itertools::Itertools;
use linya::Progress;
use log::{debug, error};
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use time::macros::format_description;
use time::OffsetDateTime;
use ubyte::ToByteUnit;

const FIFTY_MB: i64 = 52_428_800;

const FIVE_HUNDRED_MB: i64 = 524_288_000;

/// The date where Arch Linux switched compression schemes from XZ to ZSTD.
const CMPR_SWITCH: i64 = 1_577_404_800;

#[derive(FromVariants)]
pub(crate) enum Error {
    Readline(rustyline::error::ReadlineError),
    Sudo(crate::utils::SudoError),
    Pacman(crate::pacman::Error),
    Cancelled,
    NoPackages,
    NothingToDo,
    #[from_variants(skip)]
    AlreadyExists(PathBuf),
    #[from_variants(skip)]
    Delete(PathBuf),
    #[from_variants(skip)]
    ReadDir(PathBuf),
    #[from_variants(skip)]
    Stdout(std::io::Error),
    #[from_variants(skip)]
    CurrDir(std::io::Error),
    #[from_variants(skip)]
    Mkdir(PathBuf, std::io::Error),
    Date(time::error::Format),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Readline(e) => error!("{e}"),
            Error::Sudo(e) => e.nested(),
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
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Readline(_) => fl!(fll, "err-user-input"),
            Error::Sudo(e) => e.localise(fll),
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
        }
    }
}

/// Downgrade the given packages.
pub(crate) fn downgrade(
    fll: &FluentLanguageLoader,
    caches: &[&Path],
    packages: Vec<String>,
) -> Result<(), Error> {
    // Exit early if the user passed no packages.
    if packages.is_empty() {
        return Err(Error::NoPackages);
    }

    crate::utils::sudo()?;

    // --- All tarball paths for packages the user asked for --- //
    let mut tarballs: HashMap<&str, Vec<PkgPath>> = HashMap::new();
    for pp in aura_core::cache::package_paths(caches) {
        if let Some(p) = packages.iter().find(|p| p == &&pp.as_package().name) {
            let paths = tarballs.entry(p).or_insert_with(Vec::new);
            paths.push(pp);
        }
    }

    let mut to_downgrade: Vec<OsString> = packages
        .iter()
        .filter_map(|p| tarballs.remove(p.as_str()).map(|pps| (p, pps)))
        .filter_map(|(p, pps)| downgrade_one(fll, p, pps).ok())
        .map(|pp| pp.into_pathbuf().into_os_string())
        .collect();

    if to_downgrade.is_empty() {
        return Err(Error::NothingToDo);
    }

    to_downgrade.push(OsStr::new("-U").to_os_string());
    to_downgrade.reverse();

    crate::pacman::pacman(to_downgrade)?;
    green!(fll, "common-done");
    Ok(())
}

fn downgrade_one<'a>(
    fll: &FluentLanguageLoader,
    package: &str,
    mut tarballs: Vec<PkgPath<'a>>,
) -> Result<PkgPath<'a>, Error> {
    tarballs.sort_by(|a, b| b.as_package().cmp(a.as_package()));
    let digits = 1 + (tarballs.len() / 10);

    let pkg = package.bold().cyan().to_string();
    aura!(fll, "C-downgrade-which", pkg = pkg);

    for (i, pp) in tarballs.iter().enumerate() {
        println!(" {:w$}) {}", i, pp.as_package().version, w = digits);
    }

    let index = crate::utils::select(">>> ", tarballs.len() - 1)?;

    Ok(tarballs.remove(index))
}

/// Delete invalid tarballs from the cache.
pub(crate) fn invalid(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    caches: &[&Path],
) -> Result<(), Error> {
    crate::utils::sudo()?;
    aura!(fll, "C-t-invalids");

    // FIXME Thu Jan 27 15:24:44 2022
    //
    // Use `Validated` here.
    aura_core::cache::package_paths(caches)
        .filter(|pp| !aura_arch::is_valid_package(alpm, pp.as_path()))
        .for_each(|pp| {
            let _ = pp.remove(); // TODO Better handling.
        });

    green!(fll, "common-done");
    Ok(())
}

/// Print the contents of the package caches.
pub(crate) fn list(caches: &[&Path]) -> Result<(), Error> {
    for path in caches {
        path.read_dir()
            .map_err(|_| Error::ReadDir(path.to_path_buf()))?
            .filter_map(|de| de.ok())
            .for_each(|de| println!("{}", de.path().display()));
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
    let db = alpm.localdb();
    let mut w = BufWriter::new(std::io::stdout());

    let name = fl!(fll, "common-name");
    let ver = fl!(fll, "C-i-latest");
    let created = fl!(fll, "C-i-created");
    let sig = fl!(fll, "C-i-sig");
    let size = fl!(fll, "C-i-size");
    let av = fl!(fll, "C-i-avail");
    let inst = fl!(fll, "C-i-installed");

    for ci in packages
        .iter()
        .filter_map(|p| aura_core::cache::info(caches, p).ok())
        .flatten()
    {
        let dt = OffsetDateTime::from(ci.created).format(format_description!(
            "[year]-[month]-[day] [hour]-[minute]-[second]"
        ))?;
        let sig_yes_no = if ci.signature {
            fl!(fll, "common-yes").green().bold()
        } else {
            fl!(fll, "common-no").yellow()
        };
        let is_in = if let Ok(pkg) = db.pkg(ci.name.as_str()) {
            if ci.version == pkg.version().as_str() {
                format!("[{}]", inst).cyan().bold()
            } else {
                format!("[{}: {}]", inst, pkg.version()).yellow().bold()
            }
        } else {
            "".normal()
        };

        let pairs: Vec<(&str, ColoredString)> = vec![
            (&name, ci.name.normal()),
            (&ver, format!("{} {}", ci.version.normal(), is_in).normal()),
            (&created, format!("{}", dt).normal()),
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
    fll: &FluentLanguageLoader,
    caches: &[&Path],
    keep: usize,
) -> Result<(), Error> {
    crate::utils::sudo()?;

    let size_before = aura_core::cache::size(caches);
    let human = format!("{}", size_before.bytes.bytes());
    aura!(fll, "C-size", size = human);
    yellow!(fll, "C-c-keep", pkgs = keep);

    // Proceed if the user accepts.
    proceed!(fll, "proceed").ok_or(Error::Cancelled)?;

    // Get all the tarball paths, sort and group them by name, and then remove them.
    aura_core::cache::package_paths(caches)
        .sorted_by(|p0, p1| p1.cmp(p0)) // Forces a `collect` underneath.
        .group_by(|pp| pp.as_package().name.clone()) // TODO Naughty clone.
        .into_iter()
        .flat_map(|(_, group)| group.skip(keep)) // Thanks to the reverse-sort above, `group` is already backwards.
        .for_each(|pp| {
            let _ = pp.remove(); // TODO Handle this error better?
        });

    let size_after = aura_core::cache::size(caches);
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
    proceed!(fll, "proceed").ok_or(Error::Cancelled)?;

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

    for tarball in tarballs {
        let p = tarball.as_package();

        // If no snapshot contains this tarball's particular version, remove it
        // from the filesystem.
        match snaps.get(p.name.as_ref()) {
            Some(vs) if vs.contains(p.version.as_ref()) => {}
            Some(_) | None => tarball.sudo_remove().map_err(|pb| Error::Delete(pb))?,
        }
    }

    // Report the amount of disk space freed.
    let size_after = aura_core::cache::size(&caches);
    let freed = format!("{}", (size_before.bytes - size_after.bytes).bytes());
    green!(fll, "C-c-freed", bytes = freed);

    Ok(())
}

/// Download tarballs of installed packages that are missing from the cache.
pub(crate) fn refresh(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    caches: &[&Path],
) -> Result<(), Error> {
    crate::utils::sudo()?;

    // All installed packages that are missing a tarball in the cache.
    let ps: Vec<alpm::Package> = {
        let mut ps: Vec<_> = aura_core::cache::officials_missing_tarballs(alpm, caches).collect();
        ps.sort_by(|a, b| a.name().cmp(b.name()));
        ps
    };

    if ps.is_empty() {
        green!(fll, "C-y-no-work");
    } else {
        let long_n = ps.iter().map(|p| p.name().chars().count()).max().unwrap();
        let long_v = ps
            .iter()
            .map(|p| p.version().as_str().chars().count())
            .max()
            .unwrap();
        // TODO Localize.
        let p = format!("Package ({})", ps.len()).bold();
        let v = "Version".bold();
        let s = "Download Size";
        let total = colour_size(ps.iter().map(|p| p.download_size()).sum());
        let span = long_n + long_v;

        // Display a summary.
        println!("{:n$} {:v$} {}\n", p, v, s.bold(), n = long_n, v = long_v);
        for p in ps.iter() {
            let n = p.name();
            let v = p.version().as_str();
            let s = colour_size(p.download_size());
            println!("{:n$} {:v$} {:>9}", n, v, s, n = long_n, v = long_v);
        }
        println!("{:-<w$}", "-".magenta(), w = span + s.chars().count());
        println!(
            "{:w$} {:>9}\n",
            fl!(fll, "common-total").bold(),
            total,
            w = span + 1
        );

        // Proceed if the user accepts.
        proceed!(fll, "proceed").ok_or(Error::Cancelled)?;

        // Determine target cache.
        aura!(fll, "C-y-which-cache");
        for (i, cache) in caches.iter().enumerate() {
            println!(" {}) {}", i, cache.display());
        }
        let ix = crate::utils::select(">>> ", caches.len() - 1)?;
        let target_cache = caches[ix];

        // Mirrors.
        let mirrors: HashMap<&str, Vec<&str>> = alpm
            .syncdbs()
            .iter()
            .map(|db| (db.name(), db.servers().into_iter().collect()))
            .collect();

        // Syncable package values.
        let syncables: Vec<_> = ps
            .iter()
            .filter_map(|p| match (p.db(), package_tarball(p)) {
                (Some(db), Some(tarball)) => mirrors.get(db.name()).map(|ms| {
                    (
                        p.name(),
                        p.version().as_str(),
                        p.download_size(),
                        tarball,
                        ms,
                    )
                }),
                _ => None,
            })
            .collect();

        let progress = Arc::new(Mutex::new(Progress::new()));

        // Concurrently download every expected tarball.
        syncables
            .into_par_iter()
            .for_each_with(progress, |pr, (n, v, bytes, tarball, ms)| {
                let b_msg = format!("{} {}", n, v.dimmed());
                let bar = pr.lock().unwrap().bar(bytes as usize, b_msg);

                let mut res = ms.iter().filter_map(|m| {
                    let url = format!("{}/{}", m, tarball);
                    let target = target_cache.join(&tarball);
                    download_with_progress(&url, &target, Some((pr.clone(), &bar))).ok()
                });

                // If the download failed from every mirror, cancel the progress bar.
                if res.next().is_none() {
                    pr.lock().unwrap().cancel(bar);
                }
            });

        green!(fll, "common-done");
    }

    Ok(())
}

/// The form of a given `Package`'s tarball that we'd expect to find on a mirror.
///
/// May fail, since `Package`s are not guaranteed to have an architecture field.
fn package_tarball(pkg: &alpm::Package) -> Option<String> {
    let name = pkg.name();
    let ver = pkg.version().as_str();
    let arch = pkg.arch()?;
    let date = pkg.build_date();
    let ext = if date < CMPR_SWITCH { "xz" } else { "zst" };
    let tarball = format!("{}-{}-{}.pkg.tar.{}", name, ver, arch, ext);
    Some(tarball)
}

/// Colour a size string depending on the count of bytes.
fn colour_size(size: i64) -> ColoredString {
    if size >= FIVE_HUNDRED_MB {
        size.bytes().to_string().red()
    } else if size >= FIFTY_MB {
        size.bytes().to_string().yellow()
    } else {
        size.bytes().to_string().normal()
    }
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
    proceed!(fll, "proceed").ok_or(Error::Cancelled)?;

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
