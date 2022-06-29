//! All functionality involving the `-A` command.

mod build;

use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::{Finished, PathStr, ResultVoid};
use crate::{aura, green, proceed};
use alpm::Alpm;
use aura_core::Apply;
use colored::{ColoredString, Colorize};
use from_variants::FromVariants;
use i18n_embed::{fluent::FluentLanguageLoader, LanguageLoader};
use i18n_embed_fl::fl;
use linya::Progress;
use log::{debug, error, info};
use rayon::prelude::*;
use srcinfo::Srcinfo;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use time::OffsetDateTime;

#[derive(FromVariants)]
pub(crate) enum Error {
    Fetch(crate::fetch::Error),
    Dirs(crate::dirs::Error),
    Git(aura_core::git::Error),
    Build(build::Error),
    Deps(aura_core::aur::dependencies::Error<crate::fetch::Error>),
    Pacman(crate::pacman::Error),
    Env(crate::env::Error),
    Aur(aura_core::aur::Error),
    #[from_variants(skip)]
    Srcinfo(PathBuf, srcinfo::Error),
    #[from_variants(skip)]
    PathComponent(PathBuf),
    #[from_variants(skip)]
    FileOpen(PathBuf, std::io::Error),
    #[from_variants(skip)]
    FileWrite(PathBuf, std::io::Error),
    DateConv(time::error::ComponentRange),
    NoPackages,
    Cancelled,
    Stdout,
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Fetch(e) => e.nested(),
            Error::Dirs(e) => e.nested(),
            Error::Git(e) => e.nested(),
            Error::Build(e) => e.nested(),
            Error::Deps(e) => e.nested(),
            Error::Pacman(e) => e.nested(),
            Error::Env(e) => e.nested(),
            Error::Aur(e) => e.nested(),
            Error::Srcinfo(_, e) => error!("{e}"),
            Error::PathComponent(_) => {}
            Error::FileOpen(_, e) => error!("{e}"),
            Error::FileWrite(_, e) => error!("{e}"),
            Error::NoPackages => {}
            Error::Cancelled => {}
            Error::Stdout => {}
            Error::DateConv(e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Fetch(e) => e.localise(fll),
            Error::Dirs(e) => e.localise(fll),
            Error::Git(e) => e.localise(fll),
            Error::Build(e) => e.localise(fll),
            Error::Deps(e) => e.localise(fll),
            Error::Pacman(e) => e.localise(fll),
            Error::Env(e) => e.localise(fll),
            Error::Aur(e) => e.localise(fll),
            Error::Srcinfo(p, _) => fl!(fll, "err-srcinfo", file = p.utf8()),
            Error::PathComponent(p) => fl!(fll, "A-install-path-comp", path = p.utf8()),
            Error::Cancelled => fl!(fll, "common-cancelled"),
            Error::NoPackages => fl!(fll, "common-no-packages"),
            Error::Stdout => fl!(fll, "err-write"),
            Error::FileOpen(p, _) => fl!(fll, "err-file-open", file = p.utf8()),
            Error::FileWrite(p, _) => fl!(fll, "err-file-write", file = p.utf8()),
            Error::DateConv(_) => fl!(fll, "err-time-conv"),
        }
    }
}

/// View AUR package information.
pub(crate) fn info(fll: &FluentLanguageLoader, packages: &[String]) -> Result<(), Error> {
    info!("-Ai on {:?}", packages);
    let r: Vec<aura_core::faur::Package> = aura_core::faur::info(
        packages.iter().map(|s| s.as_str()),
        &crate::fetch::fetch_json,
    )?;
    let mut w = BufWriter::new(std::io::stdout());

    let repo = fl!(fll, "A-i-repo");
    let name = fl!(fll, "common-name");
    let ver = fl!(fll, "A-i-version");
    let stat = fl!(fll, "A-i-status");
    let main = fl!(fll, "A-i-maintainer");
    let proj = fl!(fll, "A-i-proj-url");
    let aur = fl!(fll, "A-i-aur-url");
    let lic = fl!(fll, "A-i-license");
    let grp = fl!(fll, "A-i-group");
    let prov = fl!(fll, "A-i-provides");
    let deps = fl!(fll, "A-i-depends");
    let make = fl!(fll, "A-i-make");
    let opts = fl!(fll, "A-i-opt");
    let check = fl!(fll, "A-i-check");
    let votes = fl!(fll, "A-i-votes");
    let pop = fl!(fll, "A-i-pop");
    let desc = fl!(fll, "A-i-desc");
    let keys = fl!(fll, "A-i-keywords");
    let sub = fl!(fll, "A-i-submitted");
    let upd = fl!(fll, "A-i-updated");

    for p in r {
        let pairs: Vec<(&str, ColoredString)> = vec![
            (&repo, "aur".magenta()),
            (
                &name,
                if p.name == p.package_base {
                    p.name.bold()
                } else {
                    format!("{} ({})", p.name.bold(), p.package_base.cyan()).normal()
                },
            ),
            (&ver, p.version.normal()),
            (
                &stat,
                match p.out_of_date {
                    None => "Up to Date".green(),
                    Some(_) => "Out of Date!".red(),
                },
            ),
            (
                &main,
                match p.maintainer {
                    None => "None".red(),
                    Some(m) => m.normal(),
                },
            ),
            (
                &proj,
                p.url.map(|m| m.cyan()).unwrap_or_else(|| "None".red()),
            ),
            (&aur, package_url(&p.name).normal()),
            (&lic, p.license.join(" ").normal()),
            (&grp, p.groups.join(" ").normal()),
            (&prov, p.provides.join(" ").normal()),
            (&deps, p.depends.join(" ").normal()),
            (&make, p.make_depends.join(" ").normal()),
            (&opts, p.opt_depends.join(" ").normal()),
            (&check, p.check_depends.join(" ").normal()),
            (&votes, format!("{}", p.num_votes).yellow()),
            (&pop, format!("{:.2}", p.popularity).yellow()),
            (
                &desc,
                p.description
                    .map(|d| d.normal())
                    .unwrap_or_else(|| "None".red()),
            ),
            (&keys, p.keywords.join(" ").cyan()),
            (&sub, package_date(p.first_submitted)?),
            (&upd, package_date(p.last_modified)?),
        ];
        crate::utils::info(&mut w, fll.current_language(), &pairs).map_err(|_| Error::Stdout)?;
        writeln!(w).map_err(|_| Error::Stdout)?;
    }

    Ok(())
}

/// Search the AUR via a search string.
///
/// Thanks to `clap`, the `terms` slice is guaranteed to be non-empty.
pub(crate) fn search(
    alpm: &Alpm,
    alpha: bool,
    rev: bool,
    limit: Option<usize>,
    quiet: bool,
    mut terms: Vec<String>,
) -> Result<(), Error> {
    debug!("Searching for: {:?}", terms);

    let db = alpm.localdb();
    let rep = "aur/".magenta();

    // Sanitize the input.
    terms.sort_unstable_by_key(|t| t.len());
    for t in terms.iter_mut() {
        t.make_ascii_lowercase();
    }

    debug!("Sanitized terms: {:?}", terms);

    let mut matches: Vec<aura_core::faur::Package> =
        aura_core::faur::search(terms.iter().map(|s| s.as_str()), &crate::fetch::fetch_json)?;

    debug!("Search matches: {}", matches.len());

    // Sort and filter the results as requested.
    if alpha {
        matches.sort_by(|a, b| a.name.cmp(&b.name));
    } else {
        matches.sort_by(|a, b| b.num_votes.cmp(&a.num_votes));
    }
    if rev {
        matches.reverse();
    }
    let to_take = limit.unwrap_or(matches.len());

    for p in matches.into_iter().take(to_take) {
        if quiet {
            println!("{}", p.name);
        } else {
            let n = p.name.bold();
            let vot = format!("{}", p.num_votes).yellow();
            let pop = format!("{:.2}", p.popularity).yellow();
            let ver = match p.out_of_date {
                Some(_) => p.version.red(),
                None => p.version.green(),
            };
            let ins = match db.pkg(p.name) {
                Err(_) => "".normal(),
                Ok(_) => "[installed]".bold(),
            };

            // TODO Search term highlighting
            println!("{}{} {} ({} | {}) {}", rep, n, ver, vot, pop, ins);
            println!("    {}", p.description.unwrap_or_default());
        }
    }

    Ok(())
}

/// View a package's PKGBUILD.
pub(crate) fn pkgbuild(pkg: &str, clone_d: &Path) -> Result<(), Error> {
    let path = aura_core::aur::clone_path_of_pkgbase(clone_d, pkg, &crate::fetch::fetch_json)?
        .join("PKGBUILD");

    let file = BufReader::new(File::open(&path).map_err(|e| Error::FileOpen(path, e))?);
    let mut out = BufWriter::new(std::io::stdout());

    file.lines()
        .filter_map(|line| line.ok())
        .try_for_each(|line| writeln!(out, "{}", line))
        .map_err(|_| Error::Stdout)?;

    Ok(())
}

/// Open a given package's AUR package in a browser.
pub(crate) fn open(package: &str) -> Result<(), crate::open::Error> {
    let url = package_url(package);
    crate::open::open(&url)
}

/// A package's URL on the AUR.
fn package_url(package: &str) -> String {
    format!("{}{}", crate::open::AUR_PKG_URL, package)
}

fn package_date(epoch: u64) -> Result<ColoredString, Error> {
    // FIXME Thu May  5 22:11:40 2022
    //
    // There is a panic risk here with the u64->i64 conversion. In practice it
    // should never come up, as the timestamps passed in should never be
    // anywhere near the [`u64::MAX`] value.
    let date = OffsetDateTime::from_unix_timestamp(epoch as i64)?.date();
    Ok(format!("{}", date).normal())
}

/// Clone the AUR repository of given packages.
pub(crate) fn clone_aur_repos(fll: &FluentLanguageLoader, pkgs: &[String]) -> Result<(), Error> {
    pkgs.par_iter()
        .map(|p| {
            let pkg = p.as_str();
            aura!(fll, "A-w", package = pkg);
            aura_core::aur::clone_aur_repo(None, p).void()
        })
        .collect::<Result<(), aura_core::git::Error>>()
        .map_err(Error::Git)?;

    green!(fll, "common-done");
    Ok(())
}

/// Pull the latest commits from every clone in the `packages` directory.
pub(crate) fn refresh(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    clone_d: &Path,
) -> Result<(), Error> {
    aura!(fll, "A-y-refreshing");

    let names = aura_arch::foreigns(alpm)
        .map(|p| p.name())
        .collect::<Vec<_>>();
    let mut progress = Progress::new();
    let clone_bar = progress.bar(names.len(), "Confirming local clones");
    let mtx = Mutex::new(progress);

    let uniques = names
        .into_par_iter()
        .map(|p| {
            let res = aura_core::aur::clone_path_of_pkgbase(clone_d, p, &crate::fetch::fetch_json);
            mtx.lock().unwrap().inc_and_draw(&clone_bar, 1);
            res
        })
        .collect::<Result<HashSet<PathBuf>, aura_core::aur::Error>>()?;

    let pull_bar = mtx
        .lock()
        .unwrap()
        .bar(uniques.len(), "Pulling latest commits");
    uniques
        .into_par_iter()
        .map(|path| {
            let res = aura_core::git::pull(&path);
            mtx.lock().unwrap().inc_and_draw(&pull_bar, 1);
            res
        })
        .collect::<Result<(), aura_core::git::Error>>()?;

    Ok(())
}

pub(crate) fn install<'a, I>(
    fll: &FluentLanguageLoader,
    env: &Env,
    raw_pkgs: I,
) -> Result<(), Error>
where
    I: IntoIterator<Item = &'a str>,
{
    let pkgs: Vec<_> = raw_pkgs.into_iter().collect();

    // Exit early if the user passed no packages.
    if pkgs.is_empty() {
        return Err(Error::NoPackages);
    }

    let pool = env.alpm_pool()?;
    aura!(fll, "A-install-deps");
    let rslv = aura_core::aur::dependencies::resolve(
        pool,
        &crate::fetch::fetch_json,
        &env.aur.clones,
        pkgs,
    )?;

    debug!("Satisfied: {:?}", rslv.satisfied);
    debug!("To install: {:?}", rslv.to_install);
    debug!("To build: {:?}", rslv.to_build);

    // --- Sort package names alphabetically --- //
    let to_install = {
        let mut v: Vec<_> = rslv.to_install.into_iter().collect();
        v.sort();
        v
    };
    let to_build = {
        let mut v: Vec<_> = rslv.to_build.into_iter().collect();
        v.sort_by(|a, b| a.name.cmp(&b.name));
        v
    };

    if to_install.is_empty().not() {
        aura!(fll, "A-install-repo-pkgs");
        to_install.iter().for_each(|p| println!(" {p}"));
    }
    aura!(fll, "A-install-aur-pkgs");
    to_build.iter().for_each(|p| println!(" {p}"));

    // Proceed if the user accepts.
    proceed!(fll, "proceed").ok_or(Error::Cancelled)?;

    // --- Determine the best build order --- //
    let order: Vec<Vec<&str>> = aura_core::aur::dependencies::build_order(&to_build)?;
    debug!("Build order: {:?}", order);

    // --- Install repo dependencies --- //
    if to_install.is_empty().not() {
        crate::pacman::pacman_install_from_repos(
            ["--asdeps", "--noconfirm"],
            to_install.iter().map(|o| o.as_ref()),
        )?;
    }

    // --- Build and install each layer of AUR packages --- //
    let is_single = to_build.len() == 1;
    for raw_layer in order.into_iter().apply(Finished::new) {
        let done = raw_layer.is_last();
        let layer = raw_layer.inner();
        let clone_paths = layer.into_iter().map(|pkg| env.aur.clones.join(pkg));
        let builts = build::build(fll, &env.aur, &env.general.editor, is_single, clone_paths)?;

        if builts.is_empty().not() {
            // FIXME Tue Jun 28 15:04:10 2022
            //
            // Chances are that this condition is wrong. It's conceivable that a
            // binary package could slip into an early installation layer. This
            // needs to be confirmed, though.
            let flags = (!done).then(|| ["--asdeps"].as_slice()).unwrap_or_default();
            let tarballs = builts.iter().flat_map(|b| &b.tarballs);
            crate::pacman::pacman_install_from_tarball(flags, tarballs)?;

            builts
                .into_iter()
                .try_for_each(|b| update_hash(&env.aur.hashes, &b.clone))?;
        }
    }

    green!(fll, "common-done");
    Ok(())
}

fn update_hash(hashes: &Path, clone: &Path) -> Result<(), Error> {
    let hash = aura_core::git::hash(clone)?;
    let base = clone
        .components()
        .last()
        .map(|c| c.as_os_str())
        .ok_or_else(|| Error::PathComponent(clone.to_path_buf()))?;

    let full = hashes.join(base);
    std::fs::write(&full, hash).map_err(|e| Error::FileWrite(full, e))?;
    Ok(())
}

/// Upgrade all installed AUR packages.
pub(crate) fn upgrade<'a>(
    fll: &FluentLanguageLoader,
    alpm: &'a Alpm,
    env: Env,
    refresh_also: bool,
) -> Result<(), Error> {
    info!("Upgrading all AUR packages.");
    debug!("Will ignore: {:?}", env.aur.ignores);

    if refresh_also {
        refresh(fll, alpm, &env.aur.clones)?;
    }

    // --- Query database for all non-repo packages --- //
    let mut foreigns: Vec<aura_core::Package<'a>> =
        aura_arch::foreigns(alpm).map(|p| p.into()).collect();
    debug!("Foreign packages: {}", foreigns.len());
    foreigns.retain(|p| env.aur.ignores.contains(p.name.as_ref()).not());
    debug!("After excluding ignores: {}", foreigns.len());

    // --- Ensure they all have local clones --- //
    aura!(fll, "A-u-fetch-info");
    let clones: HashSet<PathBuf> = foreigns
        .par_iter()
        .map(|p| p.name.as_ref())
        .map(|p| {
            aura_core::aur::clone_path_of_pkgbase(&env.aur.clones, p, &crate::fetch::fetch_json)
        })
        .collect::<Result<HashSet<_>, aura_core::aur::Error>>()?;
    debug!("Unique clones: {}", clones.len());

    // --- Compare versions to determine what to upgrade --- //
    aura!(fll, "A-u-comparing");
    info!("Reading .SRCINFO files...");
    let srcinfos = clones
        .into_par_iter()
        .map(|path| {
            let full = path.join(".SRCINFO");
            Srcinfo::parse_file(&full).map_err(|e| Error::Srcinfo(full, e))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    info!("Pulling AUR data...");
    let from_api: Vec<aura_core::faur::Package> = aura_core::faur::info(
        srcinfos.iter().map(|p| p.base.pkgbase.as_str()),
        &crate::fetch::fetch_json,
    )?;
    debug!("Packages pulled: {}", from_api.len());
    let db = alpm.localdb();
    let mut to_upgrade: Vec<(aura_core::Package<'_>, aura_core::Package<'_>)> = from_api
        .into_iter()
        .filter_map(|new| db.pkg(new.name.as_str()).ok().map(|old| (old, new)))
        .map(|(old, new)| (aura_core::Package::from(old), aura_core::Package::from(new)))
        .filter(|(old, new)| old < new)
        .collect();
    debug!("Packages to upgrade: {}", to_upgrade.len());

    // --- Account for VCS packages --- //
    let vcs: Vec<_> = if env.aur.git {
        foreigns
            .iter()
            .filter(|p| {
                let n = p.name.as_ref();
                n.ends_with("-git") || n.ends_with("-hg") || n.ends_with("-svn")
            })
            .filter(|p| to_upgrade.iter().all(|(old, _)| p.name != old.name))
            .collect()
    } else {
        Vec::new()
    };

    debug!("VCS packages to consider: {:?}", vcs);

    // --- Report --- //
    if to_upgrade.is_empty() && (env.aur.git.not() || (env.aur.git && vcs.is_empty())) {
        aura!(fll, "A-u-no-upgrades");
    } else {
        aura!(fll, "A-u-to-upgrade");
        to_upgrade.sort_by(|(a, _), (b, _)| a.name.cmp(&b.name));
        let longest_name = to_upgrade
            .iter()
            .map(|(old, _)| old.name.chars().count())
            .max()
            .unwrap_or(0);
        let longest_version = to_upgrade
            .iter()
            .map(|(old, _)| old.version.chars().count())
            .max()
            .unwrap_or(0);

        for (old, new) in to_upgrade.iter() {
            println!(
                " {:n$} :: {:v$} -> {}",
                old.name.cyan(),
                old.version.truecolor(128, 128, 128),
                new.version.bold(),
                n = longest_name,
                v = longest_version,
            );
        }

        if env.aur.git && vcs.is_empty().not() {
            aura!(fll, "A-u-git");
            for p in vcs.iter() {
                println!(" {}", p.name.cyan());
            }
        }

        let names = to_upgrade
            .iter()
            .map(|(old, _)| old.name.as_ref())
            .chain(vcs.iter().map(|p| p.name.as_ref()));
        install(fll, &env, names)?;
    }

    Ok(())
}
