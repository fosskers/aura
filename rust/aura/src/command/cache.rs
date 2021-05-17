//! All functionality involving the `-C` command.

use crate::download::download_with_progress;
use crate::error::Error;
use crate::{a, aln, aura, green, red, yellow};
use alpm::Alpm;
use aura_core::cache::{CacheSize, PkgPath};
use chrono::{DateTime, Local};
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use itertools::Itertools;
use linya::Progress;
use log::debug;
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use ubyte::ToByteUnit;

const FIFTY_MB: i64 = 52_428_800;

const FIVE_HUNDRED_MB: i64 = 524_288_000;

/// The date where Arch Linux switched compression schemes from XZ to ZSTD.
const CMPR_SWITCH: i64 = 1_577_404_800;

/// Downgrade the given packages.
pub(crate) fn downgrade(
    fll: &FluentLanguageLoader,
    cache: &Path,
    packages: Vec<String>,
) -> Result<(), Error> {
    // Exit early if the user passed no packages.
    if packages.is_empty() {
        red!(fll, "common-no-packages");
        return Err(Error::Silent);
    }

    sudo::escalate_if_needed()?;

    // --- All tarball paths for packages the user asked for --- //
    let mut tarballs: HashMap<&str, Vec<PkgPath>> = HashMap::new();
    for pp in aura_core::cache::package_paths(cache)? {
        if let Some(p) = packages.iter().find(|p| p == &&pp.to_package().name) {
            let paths = tarballs.entry(p).or_insert_with(Vec::new);
            paths.push(pp);
        }
    }

    let mut to_downgrade: Vec<OsString> = packages
        .iter()
        .filter_map(|p| tarballs.remove(p.as_str()).map(|pps| (p, pps)))
        .filter_map(|(p, pps)| downgrade_one(fll, &p, pps).ok())
        .map(|pp| PathBuf::from(pp).into_os_string())
        .collect();

    if to_downgrade.is_empty() {
        red!(fll, "common-no-work");
        return Err(Error::Silent);
    }

    to_downgrade.push(OsStr::new("-U").to_os_string());
    to_downgrade.reverse();

    crate::utils::pacman(to_downgrade)?;
    green!(fll, "common-done");
    Ok(())
}

fn downgrade_one(
    fll: &FluentLanguageLoader,
    package: &str,
    mut tarballs: Vec<PkgPath>,
) -> Result<PkgPath, Error> {
    tarballs.sort_by(|a, b| b.to_package().cmp(a.to_package()));
    let digits = 1 + (tarballs.len() / 10);

    let pkg = package.bold().cyan().to_string();
    aura!(fll, "C-downgrade-which", pkg = pkg);

    for (i, pp) in tarballs.iter().enumerate() {
        println!(" {:w$}) {}", i, pp.to_package().version, w = digits);
    }

    let index = crate::utils::select(">>> ", tarballs.len() - 1)?;

    Ok(tarballs.remove(index))
}

/// Delete invalid tarballs from the cache.
pub(crate) fn invalid(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) -> Result<(), Error> {
    sudo::escalate_if_needed()?;
    aura!(fll, "C-t-invalids");

    aura_core::cache::package_paths(cache)?
        .filter(|pp| !aura_arch::is_valid_package(alpm, pp.path()))
        .for_each(|pp| {
            let _ = pp.remove(); // TODO Better handling.
        });

    green!(fll, "common-done");
    Ok(())
}

/// Print the contents of the package cache.
pub(crate) fn list(cache: &Path) -> Result<(), Error> {
    for de in cache.read_dir()?.filter_map(|de| de.ok()) {
        println!("{}", de.path().display());
    }

    Ok(())
}

/// Print cache data for given packages.
pub(crate) fn info(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    path: &Path,
    packages: Vec<String>,
) -> Result<(), Error> {
    let db = alpm.localdb();

    packages
        .iter()
        .filter_map(|p| aura_core::cache::info(path, p).ok())
        .flatten()
        .for_each(|ci| {
            let name = fl!(fll, "common-name");
            let ver = fl!(fll, "C-i-latest");
            let created = fl!(fll, "C-i-created");
            let sig = fl!(fll, "C-i-sig");
            let size = fl!(fll, "C-i-size");
            let av = fl!(fll, "C-i-avail");
            let long = vec![&name, &ver, &created, &sig, &size, &av]
                .iter()
                .map(|s| s.len())
                .max()
                .unwrap();

            let dt = DateTime::<Local>::from(ci.created).format("%F %T");
            let is_in = if let Ok(pkg) = db.pkg(ci.name.as_str()) {
                if ci.version == pkg.version().as_str() {
                    format!("[{}]", fl!(fll, "C-i-installed")).cyan().bold()
                } else {
                    format!("[{}: {}]", fl!(fll, "C-i-installed"), pkg.version())
                        .yellow()
                        .bold()
                }
            } else {
                "".normal()
            };
            let sig_yes_no = if ci.signature {
                fl!(fll, "common-yes").green().bold()
            } else {
                fl!(fll, "common-no").yellow()
            };

            // TODO Handle non-ASCII padding.
            println!("{:w$} : {}", name.bold(), ci.name, w = long);
            println!("{:w$} : {} {}", ver.bold(), ci.version, is_in, w = long);
            println!("{:w$} : {}", created.bold(), dt, w = long);
            println!("{:w$} : {}", sig.bold(), sig_yes_no, w = long);
            println!("{:w$} : {}", size.bold(), ci.size.bytes(), w = long);
            println!("{:w$} : {}", av.bold(), ci.available.join(", "), w = long);
            println!();
        });

    Ok(())
}

/// Print all package filepaths from the cache that match some search term.
pub(crate) fn search(path: &Path, term: &str) -> Result<(), Error> {
    let matches = aura_core::cache::search(path, term)?;
    for file in matches {
        println!("{}", file.display());
    }
    Ok(())
}

/// Delete all but `keep`-many old tarballs for each package in the cache.
pub(crate) fn clean(fll: &FluentLanguageLoader, path: &Path, keep: usize) -> Result<(), Error> {
    sudo::escalate_if_needed()?;

    let size_before = aura_core::cache::size(path)?;
    let human = format!("{}", size_before.bytes.bytes());
    aura!(fll, "C-size", size = human);
    yellow!(fll, "C-c-keep", pkgs = keep);

    // Proceed if the user accepts.
    let msg = format!("{} {} ", fl!(fll, "proceed"), fl!(fll, "proceed-yes"));
    crate::utils::prompt(&a!(msg))?;

    // Get all the tarball paths, sort and group them by name, and then remove them.
    aura_core::cache::package_paths(path)?
        .sorted_by(|p0, p1| p1.cmp(&p0)) // Forces a `collect` underneath.
        .group_by(|pp| pp.to_package().name.clone()) // TODO Naughty clone.
        .into_iter()
        .map(|(_, group)| group.skip(keep)) // Thanks to the reverse-sort above, `group` is already backwards.
        .flatten()
        .for_each(|pp| {
            let _ = pp.remove(); // TODO Handle this error better?
        });

    let size_after = aura_core::cache::size(path)?;
    let freed = format!("{}", (size_before.bytes - size_after.bytes).bytes());
    green!(fll, "C-c-freed", bytes = freed);
    Ok(())
}

/// Delete only those tarballs which aren't present in a snapshot.
pub(crate) fn clean_not_saved(
    fll: &FluentLanguageLoader,
    cache: &Path,
    snapshot_dir: &Path,
) -> Result<(), Error> {
    // Report the initial size of the cache.
    let size_before = aura_core::cache::size(cache)?;
    let human = format!("{}", size_before.bytes.bytes());
    aura!(fll, "C-size", size = human);

    // Proceed if the user accepts.
    let msg = format!("{} {} ", fl!(fll, "proceed"), fl!(fll, "proceed-yes"));
    crate::utils::prompt(&a!(msg))?;

    let tarballs = aura_core::cache::package_paths(cache)?;

    // Every package across all snapshots, keyed to all unique versions present.
    let snaps: HashMap<String, HashSet<String>> = {
        let mut snaps: HashMap<_, HashSet<_>> = HashMap::new();

        for snap in aura_core::snapshot::snapshots(snapshot_dir)? {
            for (name, ver) in snap.packages.into_iter() {
                let entry = snaps.entry(name).or_default();
                entry.insert(ver);
            }
        }

        snaps
    };

    for tarball in tarballs {
        let p = tarball.to_package();

        // If no snapshot contains this tarball's particular version, remove it
        // from the filesystem.
        match snaps.get(&p.name) {
            Some(vs) if vs.contains(&p.version) => {}
            Some(_) | None => tarball.sudo_remove().ok_or(Error::MiscShell)?,
        }
    }

    // Report the amount of disk space freed.
    let size_after = aura_core::cache::size(cache)?;
    let freed = format!("{}", (size_before.bytes - size_after.bytes).bytes());
    green!(fll, "C-c-freed", bytes = freed);

    Ok(())
}

/// Download tarballs of installed packages that are missing from the cache.
pub(crate) fn refresh(fll: &FluentLanguageLoader, alpm: &Alpm, path: &Path) -> Result<(), Error> {
    sudo::escalate_if_needed()?;

    // All installed packages that are missing a tarball in the cache.
    let ps: Vec<alpm::Package> = {
        let mut ps: Vec<_> = aura_core::cache::officials_missing_tarballs(alpm, path)?.collect();
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
        let msg = format!("{} {} ", fl!(fll, "proceed"), fl!(fll, "proceed-yes"));
        crate::utils::prompt(&a!(msg))?;

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
                    let mut target = path.to_path_buf();
                    target.push(&tarball);
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

/// Backup the package cache to a given directory.
pub(crate) fn backup(
    fll: &FluentLanguageLoader,
    source: &Path,
    target: &Path,
) -> Result<(), Error> {
    // The full, absolute path to copy files to.
    let full: PathBuf = if target.is_absolute() {
        target.to_path_buf()
    } else {
        let mut curr = std::env::current_dir()?;
        curr.push(target);
        curr
    };
    let ts = full.to_str().unwrap();

    // Exit early if the target is an existing file, not a directory.
    if target.is_file() {
        red!(fll, "C-b-file", target = ts);
        return Err(Error::Silent);
    }

    // How big is the current cache?
    let cache_size: CacheSize = aura_core::cache::size(source)?;
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
    let msg = format!("{} {} ", fl!(fll, "proceed"), fl!(fll, "proceed-yes"));
    crate::utils::prompt(&a!(msg))?;
    copy(source, &full, cache_size.files)
}

/// Copy all the cache files concurrently.
fn copy(source: &Path, target: &Path, file_count: usize) -> Result<(), Error> {
    debug!("Begin cache copying.");

    // TODO Change the bar style.
    // A progress bar to display the copying progress.
    let pb = Arc::new(Mutex::new(Progress::new()));
    let bar = pb.lock().unwrap().bar(file_count, "File Copying"); // TODO Localize.

    // Silently succeeds if the directory already exists.
    std::fs::create_dir_all(target)?;

    source
        .read_dir()?
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

/// Display packages that don't have a tarball in the cache.
pub(crate) fn missing(alpm: &Alpm, cache: &Path) -> Result<(), Error> {
    for pkg in aura_core::cache::missing_tarballs(alpm, cache)? {
        println!("{} {}", pkg.name(), pkg.version());
    }

    Ok(())
}
