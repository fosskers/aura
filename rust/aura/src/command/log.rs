//! All functionality involving the `-L` command.

use crate::command::misc;
use crate::error::Error;
use aura_core as core;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use std::path::Path;
use std::process::Command;

/// Open the Pacman/ALPM log in `bat` or `less`.
pub fn view(path: &Path) -> Result<(), Error> {
    let prog = misc::viewer();
    Command::new(prog).arg(path).status().map_err(Error::IO)?;
    Ok(())
}

/// Search the Pacman log for a matching string.
pub fn search(path: &Path, term: String) -> Result<(), Error> {
    let srch = misc::searcher();
    Command::new(srch)
        .arg(term)
        .arg(path)
        .status()
        .map_err(Error::IO)?;
    Ok(())
}

/// Display install/upgrade history for the given packages.
pub fn info(fll: FluentLanguageLoader, path: &Path, pks: Vec<String>) {
    for e in pks.into_iter().filter_map(|p| core::log::info(path, p)) {
        let p = fl!(fll, "logs-package");
        let f = fl!(fll, "logs-first");
        let u = fl!(fll, "logs-upgrades");
        let r = fl!(fll, "logs-recent");
        // The longest field.
        let l = vec![&p, &f, &u, &r]
            .iter()
            .map(|s| s.chars().count())
            .max()
            .unwrap();
        // A width multiplier to aid in proper padding below.
        let lang = fll.current_language().language;
        let m = if lang == "ja" { 2 } else { 1 };
        println!("{}{:w$} : {}", p.bold(), "", e.package, w = pad(m, l, &p));
        println!("{}{:w$} : {}", f.bold(), "", e.installed, w = pad(m, l, &f));
        println!("{}{:w$} : {}", u.bold(), "", e.upgrades, w = pad(m, l, &u));
        println!("{}{:w$} :", r.bold(), "", w = pad(m, l, &r));
        for r in e.recent {
            println!("{}", r);
        }
        println!();
    }
}

fn pad(mult: usize, longest: usize, s: &str) -> usize {
    mult * (longest - s.chars().count())
}
