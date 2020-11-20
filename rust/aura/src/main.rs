use alpm::Alpm;
use aura::command::orphans;
use aura::error::Error;
use aura::flags::{SubCmd, ToArgs};
use aura_arch as arch;
use clap::Clap;
use i18n_embed::fluent::{fluent_language_loader, FluentLanguageLoader};
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use rust_embed::RustEmbed;
use std::process::Command;

#[derive(RustEmbed)]
#[folder = "i18n"]
struct Translations;

fn main() -> Result<(), Error> {
    let args = aura::flags::Args::parse();
    // let raw = aura::flags::Args::into_app().get_matches();
    let mut alpm = Alpm::new(
        args.root.unwrap_or(arch::DEFAULT_ROOT.to_string()),
        args.dbpath.unwrap_or(arch::DEFAULT_DB.to_string()),
    )
    .map_err(Error::Alpm)?;

    let loader: FluentLanguageLoader = fluent_language_loader!();
    loader
        .load_languages(&Translations, &[loader.fallback_language()])
        .unwrap();

    println!("{}", fl!(loader, "hello"));

    match args.subcmd {
        // --- Pacman Commands --- //
        SubCmd::Database(_) => unimplemented!(),
        SubCmd::Files(_) => unimplemented!(),
        SubCmd::Query(_) => unimplemented!(),
        SubCmd::Remove(_) => unimplemented!(),
        SubCmd::DepTest(_) => unimplemented!(),
        SubCmd::Upgrade(_) => unimplemented!(),
        SubCmd::Sync(s) => {
            let back = s.to_args();
            Command::new("pacman")
                .args(back)
                .status()
                .map_err(Error::IO)?;
        }
        // --- AUR Packages --- //
        SubCmd::AurSync => unimplemented!(),
        SubCmd::Backup => unimplemented!(),
        SubCmd::Cache => unimplemented!(),
        SubCmd::Log => unimplemented!(),
        SubCmd::Languages => unimplemented!(),
        SubCmd::ViewConf => unimplemented!(),
        SubCmd::Extra => unimplemented!(),
        // --- Orphan Packages --- //
        SubCmd::Orphans(o) if o.abandon => orphans::remove(&mut alpm)?,
        SubCmd::Orphans(o) if !o.adopt.is_empty() => orphans::adopt(&alpm, o.adopt)?,
        SubCmd::Orphans(_) => orphans::list(&alpm),
        // --- PKGBUILD Analysis --- //
        SubCmd::Analysis(_) => unimplemented!(),
    }

    Ok(())
}
