//! The Aura Package Manager.

#![warn(missing_docs)]

pub(crate) mod command;
pub(crate) mod download;
pub(crate) mod error;
pub(crate) mod flags;
pub(crate) mod localization;
mod macros;
pub(crate) mod utils;

use ::log::debug;
use alpm::{Alpm, SigLevel};
use clap::Clap;
use command::*;
use error::Error;
use flags::{SubCmd, AURA_GLOBALS};
use simplelog::Config;
use simplelog::TermLogger;
use simplelog::TerminalMode;
use std::process::Command;
use std::{collections::HashMap, path::Path};

fn main() -> Result<(), Error> {
    // Parse all CLI input. Exits immediately if invalid input is given.
    let args = flags::Args::parse();

    // Activate the logger.
    if let Some(l) = args.log_level {
        TermLogger::init(l, Config::default(), TerminalMode::Mixed)?;
    }

    // Establish the language strings to be used.
    let lang = args.language();
    let fll = localization::load(lang)?;

    // Parse the major configuration files.
    // TODO Consider the flag they might have given to change the conf path.
    let pconf = pacmanconf::Config::new()?;

    // Establish common file paths.
    let logp: &Path = args
        .logfile
        .as_deref()
        .unwrap_or_else(|| Path::new(&pconf.log_file));
    // TODO Allow for multiple cache paths?
    let cachep: &Path = args
        .cachedir
        .as_deref()
        .unwrap_or_else(|| Path::new(pconf.cache_dir.first().unwrap()));

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
        SubCmd::Aur(_) => unimplemented!(),
        SubCmd::Backup(_) => unimplemented!(),
        // --- The Package Cache --- //
        SubCmd::Cache(c) if !c.info.is_empty() => cache::info(fll, &alpm, cachep, c.info)?,
        SubCmd::Cache(c) if c.search.is_some() => cache::search(cachep, &c.search.unwrap())?,
        SubCmd::Cache(c) if c.backup.is_some() => cache::backup(fll, cachep, &c.backup.unwrap())?,
        SubCmd::Cache(c) if c.clean.is_some() => cache::clean(fll, cachep, c.clean.unwrap())?,
        SubCmd::Cache(c) if c.list => cache::list(cachep)?,
        SubCmd::Cache(c) if c.refresh => cache::refresh(fll, &alpm, cachep)?,
        SubCmd::Cache(_) => unimplemented!(),
        // --- Logs --- //
        SubCmd::Log(l) if l.search.is_some() => log::search(logp, l.search.unwrap())?,
        SubCmd::Log(l) if !l.info.is_empty() => log::info(fll, logp, l.info),
        SubCmd::Log(_) => log::view(logp)?,
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
    }

    Ok(())
}

/// Run a Pacman command.
fn pacman() -> Result<(), Error> {
    let mut raws: Vec<String> = std::env::args()
        .skip(1)
        .filter(|a| !(AURA_GLOBALS.contains(&a.as_str()) || a.starts_with("--log-level=")))
        .collect();

    // Special consideration for split cases like `--log-level debug`.
    if let Some(ix) = raws
        .iter()
        .enumerate()
        // TODO Use `bool::then` once it soon stabilizes.
        .find_map(|(i, v)| if v == "--log-level" { Some(i) } else { None })
    {
        raws.remove(ix); // --log-level
        raws.remove(ix); // Its argument.
    }

    debug!("Passing to Pacman: {:?}", raws);

    match Command::new("pacman").args(raws).status() {
        Err(e) => Err(Error::IO(e)),
        Ok(es) if es.success() => Ok(()),
        Ok(_) => Err(Error::PacmanError),
    }
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
