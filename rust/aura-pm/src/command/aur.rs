//! All functionality involving the `-A` command.

mod build;

use crate::aln;
use crate::aura;
use crate::env::Env;
use crate::error::Nested;
use crate::green;
use crate::localization::Localised;
use crate::proceed;
use crate::utils::Finished;
use crate::utils::PathStr;
use crate::utils::ResultVoid;
use crate::utils::NOTHING;
use crate::yellow;
use applying::Apply;
use aura_core::aur::dependencies::Resolution;
use aura_core::Package;
use colored::ColoredString;
use colored::Colorize;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use linya::Progress;
use log::debug;
use log::error;
use log::info;
use r2d2_alpm::Alpm;
use rayon::prelude::*;
use srcinfo::Srcinfo;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::ops::Not;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Mutex;
use time::OffsetDateTime;
use validated::Validated;

const AUR_PKG_URL: &str = "https://aur.archlinux.org/packages/";

pub(crate) enum Error {
    Backup(crate::command::snapshot::Error),
    Fetch(crate::fetch::Error),
    Git(aura_core::git::Error),
    Build(build::Error),
    Deps(aura_core::aur::dependencies::Error<crate::fetch::Error>),
    Pacman(crate::pacman::Error),
    Env(crate::env::Error),
    Aur(aura_core::aur::Error),
    Srcinfo(PathBuf, srcinfo::Error),
    PathComponent(PathBuf),
    FileOpen(PathBuf, std::io::Error),
    FileWrite(PathBuf, std::io::Error),
    DateConv(time::error::ComponentRange),
    NoPackages,
    Cancelled,
    Stdout,
    ReadDir(PathBuf, std::io::Error),
    CouldntOpen(String, std::io::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Fetch(e) => e.nested(),
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
            Error::Backup(e) => e.nested(),
            Error::ReadDir(_, e) => error!("{e}"),
            Error::CouldntOpen(_, e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Fetch(e) => e.localise(fll),
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
            Error::Backup(e) => e.localise(fll),
            Error::ReadDir(p, _) => fl!(fll, "err-read-dir", dir = p.utf8()),
            Error::CouldntOpen(url, _) => fl!(fll, "open-err", url = url),
        }
    }
}

/// Whether this build process is a one-off build, or part of a complete upgrade.
pub(crate) enum Mode {
    Install,
    Upgrade,
}

/// View AUR package information.
pub(crate) fn info(fll: &FluentLanguageLoader, packages: &[String]) -> Result<(), Error> {
    info!("-Ai on {:?}", packages);
    let r: Vec<aura_core::faur::Package> = aura_core::faur::info(
        packages.iter().map(|s| s.as_str()),
        &crate::fetch::fetch_json,
    )
    .map_err(Error::Fetch)?;
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

pub(crate) fn provides<S>(
    alpm: &Alpm,
    alpha: bool,
    rev: bool,
    limit: Option<usize>,
    quiet: bool,
    providing: S,
) -> Result<(), Error>
where
    S: AsRef<str>,
{
    let mut matches: Vec<aura_core::faur::Package> =
        aura_core::faur::provides(providing, &crate::fetch::fetch_json).map_err(Error::Fetch)?;

    matches.sort_by(|a, b| a.name.cmp(&b.name));

    render_search(alpm, alpha, rev, limit, quiet, matches);

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
    terms: Vec<String>,
) -> Result<(), Error> {
    debug!("Searching for: {:?}", terms);

    // Sanitize the input.
    let cleaned: HashSet<_> = terms
        .iter()
        .flat_map(|s| s.split(['-', '_']))
        .map(|s| s.to_ascii_lowercase())
        .collect();

    debug!("Sanitized terms: {:?}", cleaned);

    let matches: Vec<aura_core::faur::Package> = aura_core::faur::search(
        cleaned.iter().map(|s| s.as_str()),
        &crate::fetch::fetch_json,
    )
    .map_err(Error::Fetch)?;

    debug!("Search matches: {}", matches.len());

    render_search(alpm, alpha, rev, limit, quiet, matches);

    Ok(())
}

/// Render some search results. Orders by vote count by default.
fn render_search(
    alpm: &Alpm,
    alpha: bool,
    rev: bool,
    limit: Option<usize>,
    quiet: bool,
    mut matches: Vec<aura_core::faur::Package>,
) {
    let db = alpm.alpm.localdb();
    let rep = "aur/".magenta();

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
            println!("{rep}{n} {ver} ({vot} | {pop}) {ins}");
            println!("    {}", p.description.unwrap_or_default());
        }
    }
}

/// View a package's PKGBUILD.
pub(crate) fn pkgbuild(pkg: &str, clone_d: &Path) -> Result<(), Error> {
    let path = aura_core::aur::clone_path_of_pkgbase(clone_d, pkg, &crate::fetch::fetch_json)
        .map_err(Error::Aur)?
        .join("PKGBUILD");

    let file = BufReader::new(File::open(&path).map_err(|e| Error::FileOpen(path, e))?);
    let mut out = BufWriter::new(std::io::stdout());

    file.lines()
        .map_while(Result::ok)
        .try_for_each(|line| writeln!(out, "{line}"))
        .map_err(|_| Error::Stdout)?;

    Ok(())
}

/// Open a given package's AUR package in a browser.
pub(crate) fn open(package: &str) -> Result<(), Error> {
    let url = package_url(package);
    Command::new("xdg-open")
        .arg(&url)
        .status()
        .map_err(|e| Error::CouldntOpen(url, e))?;
    Ok(())
}

/// A package's URL on the AUR.
fn package_url(package: &str) -> String {
    format!("{AUR_PKG_URL}{package}")
}

fn package_date(epoch: u64) -> Result<ColoredString, Error> {
    // FIXME Thu May  5 22:11:40 2022
    //
    // There is a panic risk here with the u64->i64 conversion. In practice it
    // should never come up, as the timestamps passed in should never be
    // anywhere near the [`u64::MAX`] value.
    let date = OffsetDateTime::from_unix_timestamp(epoch as i64)
        .map_err(Error::DateConv)?
        .date();
    Ok(format!("{date}").normal())
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
pub(crate) fn refresh(fll: &FluentLanguageLoader, clone_d: &Path) -> Result<(), Error> {
    aura!(fll, "A-y-refreshing");

    let uniques: HashSet<_> = clone_d
        .read_dir()
        .map_err(|e| Error::ReadDir(clone_d.to_path_buf(), e))?
        .filter_map(|de| de.ok())
        .map(|de| de.path())
        .collect();

    let progress = Mutex::new(Progress::new());
    let pull_bar = progress
        .lock()
        .unwrap()
        .bar(uniques.len(), fl!(fll, "A-y-pulling"));

    if let Validated::Fail(errors) = uniques
        .into_par_iter()
        .map(|path| {
            let res = aura_core::git::pull(&path);
            progress.lock().unwrap().inc_and_draw(&pull_bar, 1);
            res
        })
        .collect::<Validated<(), aura_core::git::Error>>()
    {
        for error in errors {
            let msg = error.localise(fll);
            aln!(msg.yellow());
        }
    }

    Ok(())
}

pub(crate) fn install<'a, I>(
    fll: &FluentLanguageLoader,
    env: &Env,
    mode: Mode,
    raw_pkgs: I,
) -> Result<(), Error>
where
    I: IntoIterator<Item = &'a str>,
{
    let pkgs: HashSet<_> = raw_pkgs
        .into_iter()
        .filter(|p| {
            // Prompt if the user specified packages that are marked "ignored".
            if env.aur.ignores.contains(*p) {
                let pkg = p.bold().cyan().to_string();
                proceed!(fll, env, "A-install-ignored", file = pkg).is_some()
            } else {
                true
            }
        })
        .collect();

    // Exit early if the user passed no packages.
    if pkgs.is_empty() {
        return Err(Error::NoPackages);
    }

    // `-a` was used, or was otherwise specified in config.
    if env.aur.delmakedeps {
        let alpm = env.alpm().map_err(Error::Env)?;
        let before: HashSet<_> = aura_core::orphans(&alpm).map(|p| p.name()).collect();
        install_work(fll, env, mode, &pkgs)?;
        // Another handle must be opened, or else the change in orphan packages won't be detected.
        let alpm = env.alpm().map_err(Error::Env)?;
        let after: HashSet<_> = aura_core::orphans(&alpm).map(|p| p.name()).collect();
        let mut diff: Vec<_> = after.difference(&before).collect();
        // `base-devel` is added automatically to the build if the user didn't
        // have it installed. It would be counter-intuitive to have it removed
        // again, so we avoid that here.
        diff.retain(|p| **p != "base-devel" && pkgs.contains(**p).not());
        if diff.is_empty().not() {
            crate::pacman::sudo_pacman(env, "-Rsu", NOTHING, diff).map_err(Error::Pacman)?;
        }
    } else {
        install_work(fll, env, mode, &pkgs)?;
    }

    Ok(())
}

fn install_work(
    fll: &FluentLanguageLoader,
    env: &Env,
    mode: Mode,
    pkgs: &HashSet<&str>,
) -> Result<(), Error> {
    let pool = env.alpm_pool().map_err(Error::Env)?;
    aura!(fll, "A-install-deps");

    let rslv = if env.aur.skipdepcheck {
        Resolution::build_these(pkgs)
    } else {
        aura_core::aur::dependencies::resolve(
            pool,
            &crate::fetch::fetch_json,
            &env.aur.clones,
            env.aur.nocheck,
            pkgs,
        )
        .map_err(Error::Deps)?
    };

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

    if env.aur.noconfirm.not() {
        // Proceed if the user accepts.
        proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;
    }

    if matches!(mode, Mode::Upgrade) && env.backups.automatic {
        let alpm = env.alpm().map_err(Error::Env)?;
        crate::command::snapshot::save(fll, &alpm, env.backups.snapshots.as_path())
            .map_err(Error::Backup)?;
    }

    // --- Determine the best build order --- //
    let is_single = to_build.len() == 1;
    let order: Vec<Vec<String>> =
        aura_core::aur::dependencies::build_order(to_build).map_err(Error::Deps)?;
    debug!("Build order: {:?}", order);

    // --- Install repo dependencies --- //
    if to_install.is_empty().not() {
        crate::pacman::pacman_install_from_repos(
            env,
            ["--asdeps", "--noconfirm"],
            to_install.iter().map(|o| o.as_ref()),
        )
        .map_err(Error::Pacman)?;
    }

    // --- Build and install each layer of AUR packages --- //
    let caches = env.caches();
    let alpm = env.alpm().map_err(Error::Env)?;
    for raw_layer in order.into_iter().apply(Finished::new) {
        let done = raw_layer.is_last();
        let layer = raw_layer.inner();
        let clone_paths = layer.into_iter().map(|pkg| env.aur.clones.join(pkg));

        let builts = build::build(
            fll,
            &caches,
            env,
            &alpm,
            &env.general.editor,
            is_single,
            pkgs,
            clone_paths,
        )
        .map_err(Error::Build)?;

        if builts.is_empty().not() {
            // FIXME Tue Jun 28 15:04:10 2022
            //
            // Chances are that this condition is wrong. It's conceivable that a
            // binary package could slip into an early installation layer. This
            // needs to be confirmed, though.
            //
            // 2028-08-07 This happened to me yesterday.
            let mut flags = (!done || (matches!(mode, Mode::Install) && env.aur.asdeps))
                .then(|| vec!["--asdeps"])
                .unwrap_or_default();

            if env.general.noconfirm {
                flags.push("--noconfirm");
            }

            let tarballs = builts
                .iter()
                .flat_map(|b| b.tarballs.iter().map(|pp| pp.as_path()));
            crate::pacman::pacman_install_from_tarball(env, flags, tarballs)
                .map_err(Error::Pacman)?;

            builts
                .into_iter()
                .try_for_each(|b| update_hash(&env.aur.hashes, &b.clone))?;
        }
    }

    green!(fll, "common-done");
    Ok(())
}

fn update_hash(hashes: &Path, clone: &Path) -> Result<(), Error> {
    let hash = aura_core::git::hash(clone).map_err(Error::Git)?;
    let base = clone
        .components()
        .next_back()
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
    dryrun: bool,
) -> Result<(), Error> {
    info!("Upgrading all AUR packages.");
    debug!("Will ignore: {:?}", env.aur.ignores);

    // --- Query database for all non-repo packages --- //
    let foreigns: Vec<aura_core::Package<'a>> = aura_core::foreign_packages(alpm)
        .filter_map(aura_core::Package::from_alpm)
        .collect();
    debug!("Foreign packages: {}", foreigns.len());
    let foreign_names: HashSet<_> = foreigns.iter().map(|p| p.name.as_ref()).collect();
    let filtered: Vec<_> = foreigns
        .iter()
        .filter(|p| {
            if let Some(base) = p.name.strip_suffix("-debug") {
                foreign_names.contains(base).not()
            } else {
                true
            }
        })
        .filter(|p| env.aur.ignores.contains(p.name.as_ref()).not())
        .collect();
    debug!("After excluding ignores and debugs: {}", filtered.len());

    // --- Ensure they all have local clones --- //
    if dryrun.not() {
        aura!(fll, "A-u-fetch-info");
    }
    let clones: HashSet<PathBuf> = filtered
        .par_iter()
        .map(|p| p.name.as_ref())
        .filter_map(|p| {
            let rpath = aura_core::aur::clone_path_of_pkgbase(
                &env.aur.clones,
                p,
                &crate::fetch::fetch_json,
            );

            match rpath {
                Ok(path) => Some(Ok(path)),
                Err(aura_core::aur::Error::PackageDoesNotExist(p)) => {
                    if dryrun.not() && env.aur.warn_unknowns {
                        yellow!(fll, "faur-unknown", pkg = p);
                    }
                    None
                }
                Err(e) => Some(Err(e)),
            }
        })
        .collect::<Result<HashSet<_>, aura_core::aur::Error>>()
        .map_err(Error::Aur)?;
    debug!("Unique clones: {}", clones.len());

    // --- Compare versions to determine what to upgrade --- //
    if dryrun.not() {
        aura!(fll, "A-u-comparing");
    }
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
    )
    .map_err(Error::Fetch)?;
    debug!("Packages pulled: {}", from_api.len());
    let db = alpm.alpm.localdb();
    let mut to_upgrade: Vec<(aura_core::Package<'_>, aura_core::Package<'_>)> = from_api
        .into_iter()
        .filter_map(|new| db.pkg(new.name.as_str()).ok().map(|old| (old, new)))
        .filter_map(
            |(old, new)| match (Package::from_alpm(old), Package::from_faur(new)) {
                (Some(o), Some(n)) => Some((o, n)),
                _ => None,
            },
        )
        .filter(|(old, new)| old < new)
        .collect();
    debug!("Packages to upgrade: {}", to_upgrade.len());

    // --- Account for VCS packages --- //
    let vcs: Vec<_> = if env.aur.git {
        filtered
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
        if dryrun.not() {
            aura!(fll, "A-u-no-upgrades");
        }
    } else {
        if dryrun.not() {
            aura!(fll, "A-u-to-upgrade");
        }
        to_upgrade.sort_by(|(a, _), (b, _)| a.name.cmp(&b.name));
        let longest_name = to_upgrade
            .iter()
            .map(|(old, _)| old.name.chars().count())
            .max()
            .unwrap_or(0);
        let longest_version = to_upgrade
            .iter()
            .map(|(old, _)| old.version.to_string().chars().count())
            .max()
            .unwrap_or(0);

        for (old, new) in to_upgrade.iter() {
            println!(
                " {:n$} :: {:v$} -> {}",
                old.name.cyan(),
                old.version.to_string().truecolor(128, 128, 128),
                new.version.to_string().bold(),
                n = longest_name,
                v = longest_version,
            );
        }

        // We've printed the packages that have available upgrades, so now bail
        // early before anything else can happen.
        if dryrun {
            return Ok(());
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

        install(fll, &env, Mode::Upgrade, names)?;
    }

    Ok(())
}
