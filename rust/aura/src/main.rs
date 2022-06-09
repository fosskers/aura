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

use ::log::debug;
use clap::Parser;
use command::{aur, cache, check, conf, deps, log, open, orphans, snapshot, stats};
use error::Error;
use flags::{Args, Cache, SubCmd, AURA_GLOBALS};
use simplelog::{ColorChoice, Config, TermLogger, TerminalMode};
use std::ops::Not;
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

    // --- Runtime Settings --- //
    let env = {
        let mut env = crate::env::Env::try_new()?;
        env.reconcile_cli(&args.subcmd);
        env
    };
    debug!("{:#?}", env);

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
            aur::search(&env.alpm()?, a.abc, a.reverse, a.limit, a.quiet, a.search)?
        }
        SubCmd::Aur(a) if a.open.is_some() => aur::open(&a.open.unwrap())?,
        SubCmd::Aur(a) if a.pkgbuild.is_some() => {
            aur::pkgbuild(&a.pkgbuild.unwrap(), &env.aur.clones)?
        }
        SubCmd::Aur(a) if a.wclone.is_empty().not() => aur::clone_aur_repos(&fll, &a.wclone)?,
        SubCmd::Aur(a) if a.refresh => aur::refresh(&fll, &env.alpm()?, &env.aur.clones)?,
        SubCmd::Aur(a) if a.sysupgrade => aur::upgrade(&fll, &env.alpm()?, env)?,
        SubCmd::Aur(a) => aur::install(&fll, &env, a.packages.iter().map(|s| s.as_str()))?,
        // --- Package Sets --- //
        SubCmd::Backup(b) if b.clean => {
            snapshot::clean(&fll, &env.caches(), &env.backups.snapshots)?
        }
        SubCmd::Backup(b) if b.list => snapshot::list(&env.backups.snapshots)?,
        SubCmd::Backup(b) if b.restore => {
            snapshot::restore(&fll, &env.alpm()?, &env.caches(), &env.backups.snapshots)?
        }
        SubCmd::Backup(_) => snapshot::save(&fll, &env.alpm()?, &env.backups.snapshots)?,
        // --- Cache Management --- //
        SubCmd::Cache(c) if !c.info.is_empty() => {
            cache::info(&fll, &env.alpm()?, &env.caches(), c.info)?
        }
        SubCmd::Cache(c) if c.search.is_some() => cache::search(&env.caches(), &c.search.unwrap())?,
        SubCmd::Cache(c) if c.backup.is_some() => cache::backup(&fll, &env, &c.backup.unwrap())?,
        SubCmd::Cache(Cache { clean: Some(n), .. }) => cache::clean(&fll, &env.caches(), n)?,
        SubCmd::Cache(c) if c.clean_unsaved => cache::clean_not_saved(&fll, &env)?,
        SubCmd::Cache(c) if c.invalid => cache::invalid(&fll, &env.alpm()?, &env.caches())?,
        SubCmd::Cache(c) if c.list => cache::list(&env.caches())?,
        SubCmd::Cache(c) if c.refresh => cache::refresh(&fll, &env.alpm()?, &env.caches())?,
        SubCmd::Cache(c) if c.missing => cache::missing(&env.alpm()?, &env.caches()),
        SubCmd::Cache(c) => cache::downgrade(&fll, &env.caches(), c.packages)?,
        // --- Logs --- //
        SubCmd::Log(l) if l.search.is_some() => log::search(env.alpm_log(), l.search.unwrap())?,
        SubCmd::Log(l) if !l.info.is_empty() => log::info(fll, env.alpm_log(), l.info)?,
        SubCmd::Log(l) => log::view(env.alpm_log(), l.before, l.after)?,
        // --- Orphan Packages --- //
        SubCmd::Orphans(o) if o.abandon => orphans::remove(&mut env.alpm()?, fll)?,
        SubCmd::Orphans(o) if !o.adopt.is_empty() => orphans::adopt(&env.alpm()?, fll, o.adopt)?,
        SubCmd::Orphans(_) => orphans::list(&env.alpm()?),
        // --- PKGBUILD Analysis --- //
        // SubCmd::Analysis(_) => unimplemented!(),
        // --- Configuration --- //
        SubCmd::Conf(c) if c.pacman => conf::pacman_conf(c)?,
        SubCmd::Conf(c) if c.aura => conf::aura_conf()?,
        SubCmd::Conf(c) if c.makepkg => conf::makepkg_conf()?,
        SubCmd::Conf(c) if c.gen => conf::gen(&env)?,
        SubCmd::Conf(_) => conf::general(&env),
        // --- Statistics --- //
        SubCmd::Stats(s) if s.lang => stats::localization()?,
        SubCmd::Stats(s) if s.heavy => stats::heavy_packages(&env.alpm()?),
        SubCmd::Stats(s) if s.groups => stats::groups(&env.alpm()?),
        SubCmd::Stats(_) => unimplemented!(),
        // --- Opening Webpages --- //
        SubCmd::Open(o) if o.docs => open::book()?,
        SubCmd::Open(o) if o.repo => open::repo()?,
        SubCmd::Open(o) if o.bug => open::bug()?,
        SubCmd::Open(o) if o.aur => open::aur()?,
        SubCmd::Open(_) => open::repo()?,
        // --- Dependency Management --- //
        SubCmd::Deps(d) if d.reverse => {
            deps::reverse(&env.alpm()?, d.limit, d.optional, d.packages)?
        }
        SubCmd::Deps(d) => deps::graph(&env.alpm()?, d.limit, d.optional, d.packages)?,
        // --- System Validation --- //
        SubCmd::Check(_) => check::check(&fll, &env)?,
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
