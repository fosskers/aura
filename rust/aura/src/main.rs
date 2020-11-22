//! The Aura Package Manager.

use alpm::Alpm;
use aura::command::orphans;
use aura::error::Error;
use aura::flags::SubCmd;
use aura_arch as arch;
use clap::Clap;
use std::process::Command;

fn main() -> Result<(), Error> {
    // Parse all CLI input. Exits immediately if invalid input is given.
    let args = aura::flags::Args::parse();

    // Establish the language strings to be used.
    let lang = args.language();
    let fll = aura::localization::loader(lang).map_err(Error::I18n)?;

    let mut alpm = Alpm::new(
        args.root.unwrap_or(arch::DEFAULT_ROOT.to_string()),
        args.dbpath.unwrap_or(arch::DEFAULT_DB.to_string()),
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
        SubCmd::Log => unimplemented!(),
        SubCmd::Languages => unimplemented!(),
        SubCmd::ViewConf => unimplemented!(),
        // --- Orphan Packages --- //
        SubCmd::Orphans(o) if o.abandon => orphans::remove(&mut alpm, fll)?,
        SubCmd::Orphans(o) if !o.adopt.is_empty() => orphans::adopt(&alpm, fll, o.adopt)?,
        SubCmd::Orphans(_) => orphans::list(&alpm),
        // --- PKGBUILD Analysis --- //
        SubCmd::Analysis(_) => unimplemented!(),
    }

    Ok(())
}

/// Run a Pacman command.
fn pacman() -> Result<(), Error> {
    let raw = std::env::args().skip(1);
    Command::new("pacman")
        .args(raw)
        .status()
        .map_err(Error::IO)?;
    Ok(())
}
