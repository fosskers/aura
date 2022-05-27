//! The Aura Package Manager.

#![warn(missing_docs)]

pub(crate) mod command;
pub(crate) mod dirs;
pub(crate) mod download;
pub(crate) mod env;
pub(crate) mod error;
pub(crate) mod fetch;
pub(crate) mod flags;
pub(crate) mod localization;
mod macros;
pub(crate) mod pacman;
pub(crate) mod utils;

use ::log::info;
use alpm::{Alpm, SigLevel};
use clap::Parser;
use command::{aur, cache, check, conf, deps, log, open, orphans, snapshot, stats};
use error::Error;
use flags::{Args, SubCmd, AURA_GLOBALS};
use simplelog::{ColorChoice, Config, TermLogger, TerminalMode};
use std::collections::HashMap;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

fn main() -> ExitCode {
    // Parse all CLI input. Exits immediately if invalid input is given.
    let args = flags::Args::parse();

    match work(args) {
        Err(e) => {
            // TODO Sat Jan 15 16:53:12 2022
            //
            // Localise the error messages.
            eprintln!("{}", e);
            ExitCode::FAILURE
        }
        Ok(_) => ExitCode::SUCCESS,
    }
}

fn work(args: Args) -> Result<(), Error> {
    // --- Terminal Logging --- //
    if let Some(l) = args.log_level {
        TermLogger::init(l, Config::default(), TerminalMode::Mixed, ColorChoice::Auto)?;
    }

    // --- Localisation --- //
    let lang = args.language();
    let fll = localization::load(lang)?;

    // Parse the major configuration files.
    // TODO Consider the flag they might have given to change the conf path.
    let pconf = pacmanconf::Config::new()?;

    // --- ALPM/Pacman Logging --- //
    let logp: &Path = args
        .logfile
        .as_deref()
        .unwrap_or_else(|| Path::new(&pconf.log_file));

    // --- Tarball Caches --- //
    let aur_cache = crate::dirs::tarballs()?;
    let caches: Vec<&Path> = args
        .cachedir
        .as_deref()
        .map(|d| vec![d, &aur_cache])
        .unwrap_or_else(|| {
            pconf
                .cache_dir
                .iter()
                .map(Path::new)
                .chain(std::iter::once(aur_cache.as_ref()))
                .collect()
        });
    info!("Tarball Caches: {:?}", caches);

    // --- Package Set Snapshots --- //
    let snapshots: PathBuf = dirs::snapshot()?;

    // --- ALPM Handle --- //
    let root = args.root.unwrap_or_else(|| pconf.root_dir.clone());
    let dbpath = args.dbpath.unwrap_or_else(|| pconf.db_path.clone());
    let mut alpm = alpm(&pconf, root, dbpath)?;

    match args.subcmd {
        // --- Pacman Commands --- //
        SubCmd::Database(_) => pacman()?,
        SubCmd::Files(_) => pacman()?,
        SubCmd::Query(_) => pacman()?,
        SubCmd::Remove(_) => pacman()?,
        SubCmd::DepTest(_) => pacman()?,
        SubCmd::Upgrade(_) => pacman()?,
        SubCmd::Sync(_) => pacman()?,
        // --- AUR Packages --- //
        SubCmd::Aur(a) if a.info.is_empty().not() => aur::info(&fll, &a.info)?,
        SubCmd::Aur(a) if a.search.is_empty().not() => {
            aur::search(&alpm, a.abc, a.reverse, a.limit, a.quiet, a.search)?
        }
        SubCmd::Aur(a) if a.open.is_some() => aur::open(&a.open.unwrap())?,
        SubCmd::Aur(a) if a.pkgbuild.is_some() => aur::pkgbuild(&a.pkgbuild.unwrap())?,
        SubCmd::Aur(a) if a.wclone.is_empty().not() => aur::clone_aur_repos(&fll, &a.wclone)?,
        SubCmd::Aur(a) if a.refresh => aur::refresh(&fll, &alpm)?,
        SubCmd::Aur(a) if a.sysupgrade => aur::upgrade(&fll, &alpm, pconf, a.ignore, a.git)?,
        SubCmd::Aur(a) => aur::install(&fll, pconf, a.packages.iter().map(|s| s.as_str()))?,
        // --- Package Sets --- //
        SubCmd::Backup(b) if b.clean => snapshot::clean(&fll, &caches)?,
        SubCmd::Backup(b) if b.list => snapshot::list()?,
        SubCmd::Backup(b) if b.restore => snapshot::restore(&fll, &alpm, &caches)?,
        SubCmd::Backup(_) => snapshot::save(&fll, &alpm)?,
        // --- Cache Management --- //
        SubCmd::Cache(c) if !c.info.is_empty() => cache::info(&fll, &alpm, &caches, c.info)?,
        SubCmd::Cache(c) if c.search.is_some() => cache::search(&caches, &c.search.unwrap())?,
        SubCmd::Cache(c) if c.backup.is_some() => cache::backup(&fll, &caches, &c.backup.unwrap())?,
        SubCmd::Cache(c) if c.clean.is_some() => cache::clean(&fll, &caches, c.clean.unwrap())?,
        SubCmd::Cache(c) if c.clean_unsaved => cache::clean_not_saved(&fll, &caches, &snapshots)?,
        SubCmd::Cache(c) if c.invalid => cache::invalid(&fll, &alpm, &caches)?,
        SubCmd::Cache(c) if c.list => cache::list(&caches)?,
        SubCmd::Cache(c) if c.refresh => cache::refresh(&fll, &alpm, &caches)?,
        SubCmd::Cache(c) if c.missing => cache::missing(&alpm, &caches),
        SubCmd::Cache(c) => cache::downgrade(&fll, &caches, c.packages)?,
        // --- Logs --- //
        SubCmd::Log(l) if l.search.is_some() => log::search(logp, l.search.unwrap())?,
        SubCmd::Log(l) if !l.info.is_empty() => log::info(fll, logp, l.info)?,
        SubCmd::Log(l) => log::view(logp, l.before, l.after)?,
        // --- Orphan Packages --- //
        SubCmd::Orphans(o) if o.abandon => orphans::remove(&mut alpm, fll)?,
        SubCmd::Orphans(o) if !o.adopt.is_empty() => orphans::adopt(&alpm, fll, o.adopt)?,
        SubCmd::Orphans(_) => orphans::list(&alpm),
        // --- PKGBUILD Analysis --- //
        SubCmd::Analysis(_) => unimplemented!(),
        // --- Configuration --- //
        SubCmd::Conf(c) if c.pacman => conf::pacman_conf(c)?,
        SubCmd::Conf(c) if c.aura => unimplemented!(),
        SubCmd::Conf(c) if c.makepkg => conf::makepkg_conf()?,
        SubCmd::Conf(_) => conf::general(&alpm),
        // --- Statistics --- //
        SubCmd::Stats(s) if s.lang => stats::localization()?,
        SubCmd::Stats(s) if s.heavy => stats::heavy_packages(&alpm),
        SubCmd::Stats(s) if s.groups => stats::groups(&alpm),
        SubCmd::Stats(_) => unimplemented!(),
        // --- Opening Webpages --- //
        SubCmd::Open(o) if o.docs => open::book()?,
        SubCmd::Open(o) if o.repo => open::repo()?,
        SubCmd::Open(o) if o.bug => open::bug()?,
        SubCmd::Open(o) if o.aur => open::aur()?,
        SubCmd::Open(_) => open::repo()?,
        // --- Dependency Management --- //
        SubCmd::Deps(d) if d.reverse => deps::reverse(&alpm, d.limit, d.optional, d.packages)?,
        SubCmd::Deps(d) => deps::graph(&alpm, d.limit, d.optional, d.packages)?,
        // --- System Validation --- //
        SubCmd::Check(_) => check::check(&fll, &alpm, &pconf, &caches, &snapshots),
    }

    Ok(())
}

/// Run a Pacman command.
fn pacman() -> Result<(), crate::pacman::Error> {
    let mut raws: Vec<String> = std::env::args()
        .skip(1)
        .filter(|a| !(AURA_GLOBALS.contains(&a.as_str()) || a.starts_with("--log-level=")))
        .collect();

    // Special consideration for split cases like `--log-level debug`.
    if let Some(ix) = raws
        .iter()
        .enumerate()
        .find_map(|(i, v)| (v == "--log-level").then(|| i))
    {
        raws.remove(ix); // --log-level
        raws.remove(ix); // Its argument.
    }

    ::log::debug!("Passing to Pacman: {:?}", raws);
    pacman::pacman(raws)
}

/// Fully initialize an ALPM handle.
fn alpm(conf: &pacmanconf::Config, root: String, dbpath: String) -> Result<Alpm, Error> {
    let mut alpm = Alpm::new(root, dbpath)?;
    let mirrors: HashMap<_, _> = conf
        .repos
        .iter()
        .map(|r| (r.name.as_str(), r.servers.as_slice()))
        .collect();

    // Register official "sync" databases. Without this step, the ALPM handle
    // can't access the non-local databases.
    for repo in conf.repos.iter() {
        // TODO If I ever plan to install packages manually, get the proper
        // SigLevel from `pacman.conf`.
        alpm.register_syncdb(repo.name.as_str(), SigLevel::USE_DEFAULT)?;
    }

    // Associate each registered sync db with mirrors pulled from `pacman.conf`.
    for db in alpm.syncdbs_mut() {
        if let Some(ms) = mirrors.get(db.name()) {
            for mirror in ms.iter() {
                db.add_server(mirror.as_str())?;
            }
        }
    }

    Ok(alpm)
}
