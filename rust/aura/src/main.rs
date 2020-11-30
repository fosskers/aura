//! The Aura Package Manager.

#![warn(missing_docs)]

pub mod command;
pub mod error;
pub mod flags;
pub mod localization;
mod macros;
pub mod utils;

use crate::error::Silent;
use ::log::debug;
use alpm::Alpm;
use anyhow::{ensure, Context, Result};
use clap::Clap;
use colored::Colorize;
use command::*;
use flags::{SubCmd, AURA_GLOBALS};
use simplelog::Config;
use simplelog::TermLogger;
use simplelog::TerminalMode;
use std::error::Error;
use std::path::Path;
use std::process::Command;

fn main() {
    if let Err(err) = run() {
        let mut iter = err.chain().peekable();

        if Error::is::<Silent>(*iter.peek().unwrap()) {
            eprint!("{}", iter.peek().unwrap());
            return;
        }

        eprint!("{} ", "error:".red());
        while let Some(link) = iter.next() {
            eprint!("{}", link);
            if iter.peek().is_some() {
                eprint!(": ");
            }
        }
        eprintln!();
    }
}

fn run() -> Result<()> {
    // Parse all CLI input. Exits immediately if invalid input is given.
    let args = flags::Args::parse();

    // Activate the logger.
    if let Some(l) = args.log_level {
        TermLogger::init(l, Config::default(), TerminalMode::Mixed)
            .context("failed to initialise logger")?;
    }

    // Establish the language strings to be used.
    let lang = args.language();
    let fll = localization::load(lang).context("failed to load localization")?;

    // Parse the major configuration files.
    // TODO Consider the flag they might have given to change the conf path.
    let pconf = pacmanconf::Config::new().context("failed to parse pacman.conf")?;

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

    let root = args.root.as_deref().unwrap_or(&pconf.root_dir);
    let dbpath = args.dbpath.as_deref().unwrap_or(&pconf.db_path);
    let mut alpm = Alpm::new(root, dbpath).with_context(|| {
        format!(
            "failed to initialize alpm: root='{}' dbpath='{}'",
            root, dbpath
        )
    })?;

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
        SubCmd::Cache(c) if c.search.is_some() => cache::search(cachep, c.search.unwrap())?,
        SubCmd::Cache(c) if c.backup.is_some() => cache::backup(fll, cachep, &c.backup.unwrap())?,
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
        SubCmd::Conf(_) => unimplemented!(),
        // --- Statistics --- //
        SubCmd::Stats(s) if s.localization => stats::localization()?,
        SubCmd::Stats(s) if s.heavy => stats::heavy_packages(&alpm),
        SubCmd::Stats(_) => unimplemented!(),
        // --- Opening Webpages --- //
        SubCmd::Open(o) if o.docs => open::book()?,
        SubCmd::Open(o) if o.repo => open::repo()?,
        SubCmd::Open(o) if o.bug => open::bug()?,
        SubCmd::Open(o) if o.aur => open::aur()?,
        SubCmd::Open(_) => open::repo()?,
        // --- Other --- //
        SubCmd::Lang(_) => misc::languages(),
    }

    Ok(())
}

/// Run a Pacman command.
fn pacman() -> Result<()> {
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

    let status = Command::new("pacman")
        .args(raws)
        .status()
        .context("could not exec pacman")?;
    ensure!(status.success(), "pacman failed to run");
    Ok(())
}
