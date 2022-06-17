//! All functionality involving the `-L` command.

#![allow(clippy::many_single_char_names)]

use crate::command::misc;
use crate::localization::Localised;
use crate::utils::ResultVoid;
use aura_core as core;
use chrono::NaiveDate;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;
use std::process::Command;

pub(crate) enum Error {
    Search(&'static str, std::io::Error),
    View(std::io::Error),
    Info(std::io::Error),
}

impl Error {
    pub(crate) fn nested(&self) {
        match self {
            Error::Search(_, e) => error!("{e}"),
            Error::View(e) => error!("{e}"),
            Error::Info(e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            // FIXME Fri Jun 17 14:47:05 2022
            //
            // Strange clone, or else Fluent complains about a &&str.
            Error::Search(s, _) => fl!(fll, "L-search-err", cmd = s.clone()),
            Error::View(_) => fl!(fll, "L-view-err"),
            Error::Info(_) => fl!(fll, "err-write"),
        }
    }
}

/// Search the Pacman log for a matching string.
pub(crate) fn search(path: &Path, term: String) -> Result<(), Error> {
    let (search, args) = misc::searcher();
    Command::new(search)
        .args(args)
        .arg(term)
        .arg(path)
        .status()
        .map_err(|e| Error::Search(search, e))
        .void()
}

/// Display install/upgrade history for the given packages.
pub(crate) fn info(fll: &FluentLanguageLoader, path: &Path, pks: Vec<String>) -> Result<(), Error> {
    info_work(fll, path, pks).map_err(Error::Info)
}

fn info_work(
    fll: &FluentLanguageLoader,
    path: &Path,
    pks: Vec<String>,
) -> Result<(), std::io::Error> {
    let mut w = BufWriter::new(std::io::stdout());

    let p = fl!(fll, "common-name");
    let f = fl!(fll, "L-first");
    let u = fl!(fll, "L-upgrades");
    let r = fl!(fll, "L-recent");

    for e in pks.into_iter().filter_map(|p| core::log::info(path, p)) {
        let pairs: Vec<(&str, ColoredString)> = vec![
            (&p, e.package.normal()),
            (&f, e.installed.normal()),
            (&u, format!("{}", e.upgrades).normal()),
            (&r, "".normal()),
        ];

        crate::utils::info(&mut w, fll.current_language(), &pairs)?;
        for r in e.recent {
            writeln!(w, "{}", r)?;
        }
        writeln!(w)?;
    }

    Ok(())
}

/// Output the content of the Pacman/ALPM log, possibly filtered by date.
pub(crate) fn view(
    path: &Path,
    before: Option<NaiveDate>,
    after: Option<NaiveDate>,
) -> Result<(), Error> {
    view_work(path, before, after).map_err(Error::View)
}

fn view_work(
    path: &Path,
    before: Option<NaiveDate>,
    after: Option<NaiveDate>,
) -> Result<(), std::io::Error> {
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
