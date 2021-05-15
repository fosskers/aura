//! All functionality involving the `-L` command.

use crate::command::misc;
use crate::error::Error;
use crate::utils;
use aura_core as core;
use chrono::NaiveDate;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;
use std::process::Command;

/// Search the Pacman log for a matching string.
pub(crate) fn search(path: &Path, term: String) -> Result<(), Error> {
    let (search, args) = misc::searcher();
    Command::new(search)
        .args(args)
        .arg(term)
        .arg(path)
        .status()?;
    Ok(())
}

/// Display install/upgrade history for the given packages.
pub(crate) fn info(fll: FluentLanguageLoader, path: &Path, pks: Vec<String>) {
    for e in pks.into_iter().filter_map(|p| core::log::info(path, p)) {
        let p = fl!(fll, "common-name");
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
        println!(
            "{}{:w$} : {}",
            p.bold(),
            "",
            e.package,
            w = utils::pad(m, l, &p)
        );
        println!(
            "{}{:w$} : {}",
            f.bold(),
            "",
            e.installed,
            w = utils::pad(m, l, &f)
        );
        println!(
            "{}{:w$} : {}",
            u.bold(),
            "",
            e.upgrades,
            w = utils::pad(m, l, &u)
        );
        println!("{}{:w$} :", r.bold(), "", w = utils::pad(m, l, &r));
        for r in e.recent {
            println!("{}", r);
        }
        println!();
    }
}

/// Output the content of the Pacman/ALPM log, possibly filtered by date.
pub(crate) fn view(
    path: &Path,
    before: Option<NaiveDate>,
    after: Option<NaiveDate>,
) -> Result<(), Error> {
    let file = BufReader::new(File::open(path)?);
    let mut out = BufWriter::new(std::io::stdout());

    for rline in file.lines() {
        let line = rline?;
        if let Ok(date) = line
            .chars()
            .skip(1)
            .take(10)
            .collect::<String>() // TODO Avoid the collect somehow?
            .parse::<NaiveDate>()
        {
            match (after, before) {
                (Some(a), Some(b)) if date >= a && date < b => writeln!(out, "{}", line)?,
                (Some(a), None) if date >= a => writeln!(out, "{}", line)?,
                (None, Some(b)) if date < b => writeln!(out, "{}", line)?,
                (None, None) => writeln!(out, "{}", line)?,
                (_, _) => {}
            }
        }
    }

    Ok(())
}
