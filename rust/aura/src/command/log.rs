//! Viewing the Pacman/ALPM log.

use crate::command::misc;
use crate::error::Error;
use aura_core as core;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
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
        let longest = vec![&p, &f, &u, &r].iter().map(|s| s.len()).max().unwrap();
        println!("{:w$} : {}", p.bold(), e.package, w = longest);
        println!("{:w$} : {}", f.bold(), e.installed, w = longest);
        println!("{:w$} : {}", u.bold(), e.upgrades, w = longest);
        println!("{:w$} :", r.bold(), w = longest);
        for r in e.recent {
            println!("{}", r);
        }
        println!("");
    }
}
