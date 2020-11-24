//! The Aura Package Manager.

use alpm::Alpm;
use aura::command::{log, misc, orphans};
use aura::error::Error;
use aura::flags::SubCmd;
use aura_arch as arch;
use clap::Clap;
use std::path::Path;
use std::process::Command;

fn main() -> Result<(), Error> {
    // Parse all CLI input. Exits immediately if invalid input is given.
    let args = aura::flags::Args::parse();

    // Establish the language strings to be used.
    let lang = args.language();
    let fll = aura::localization::loader(lang).map_err(Error::I18n)?;

    // Establish common file paths.
    let log_path: &Path = args
        .logfile
        .as_ref()
        .map(|p| Path::new(p))
        .unwrap_or_else(|| Path::new(arch::DEFAULT_LOG));

    let mut alpm = Alpm::new(
        args.root.unwrap_or_else(|| arch::DEFAULT_ROOT.to_string()),
        args.dbpath.unwrap_or_else(|| arch::DEFAULT_DB.to_string()),
    )
    .map_err(Error::Alpm)?;

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
        SubCmd::AurSync => unimplemented!(),
        SubCmd::Backup => unimplemented!(),
        SubCmd::Cache => unimplemented!(),
        // --- Logs --- //
        SubCmd::Log(l) if l.search.is_some() => log::search(log_path, l.search.unwrap())?,
        SubCmd::Log(l) if !l.info.is_empty() => log::info(log_path, l.info),
        SubCmd::Log(_) => log::view(log_path)?,
        // --- Orphan Packages --- //
        SubCmd::Orphans(o) if o.abandon => orphans::remove(&mut alpm, fll)?,
        SubCmd::Orphans(o) if !o.adopt.is_empty() => orphans::adopt(&alpm, fll, o.adopt)?,
        SubCmd::Orphans(_) => orphans::list(&alpm),
        // --- PKGBUILD Analysis --- //
        SubCmd::Analysis(_) => unimplemented!(),
        // --- Configuration --- //
        SubCmd::Conf(_) => unimplemented!(),
        SubCmd::PacConf(pc) => misc::pacman_conf(pc)?,
        // --- Other --- //
        SubCmd::Languages(_) => misc::languages(),
    }

    Ok(())
}

/// Run a Pacman command.
fn pacman() -> Result<(), Error> {
    let raw = std::env::args().skip(1);
    match Command::new("pacman").args(raw).status() {
        Err(e) => Err(Error::IO(e)),
        Ok(es) if es.success() => Ok(()),
        Ok(_) => Err(Error::PacmanError),
    }
}
