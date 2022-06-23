//! Statistics about the user's machine or about Aura itself.

use crate::error::Nested;
use crate::localization::{self, Localised};
use alpm::Alpm;
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use std::collections::{HashMap, HashSet};
use ubyte::ToByteUnit;
use unic_langid::{langid, LanguageIdentifier};

#[derive(FromVariants)]
pub(crate) enum Error {
    LangLoad(i18n_embed::I18nEmbedError),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::LangLoad(e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::LangLoad(_) => fl!(fll, "stats-local"),
        }
    }
}

/// Raw contents of loaded localizations.
pub(crate) fn localization() -> Result<(), Error> {
    let stats: HashMap<LanguageIdentifier, (String, usize)> = localization::load_all()?
        .into_iter()
        .map(|(lang, fll)| {
            let count = fll.with_message_iter(&lang, |iter| iter.count());
            let name = fl!(fll, "language-name");
            (lang, (name, count))
        })
        .collect();

    let (_, max) = *stats.get(&langid!("en-US")).unwrap();
    let mut sorted: Vec<_> = stats.into_iter().collect();
    sorted.sort_by_key(|(_, (_, count))| *count);
    sorted.reverse();
    let long = sorted
        .iter()
        .map(|(_, (n, _))| n.chars().count())
        .max()
        .unwrap_or(0);

    // TODO Make this a proper table with generalized table code.
    for (lang, (n, c)) in sorted {
        let perc = 100.0 * c as f64 / max as f64;
        let l = if perc < 50.0 {
            format!("{}", lang).red()
        } else if perc < 100.0 {
            format!("{}", lang).yellow()
        } else {
            format!("{}", lang).green()
        };
        let pad = long - visual_len(&lang, &n);
        println!(
            "{} [{}]{:w$} {:03}/{} ({:.2}%)",
            l,
            n,
            "",
            c,
            max,
            perc,
            w = pad
        );
    }

    Ok(())
}

fn visual_len(lang: &LanguageIdentifier, msg: &str) -> usize {
    let raw = msg.chars().count();
    match lang.language.as_str() {
        "ja" => raw * 2,
        _ => raw,
    }
}

/// Display the Top 10 packages with the biggest installation footprint.
pub(crate) fn heavy_packages(alpm: &Alpm) {
    let db = alpm.localdb();
    let mut sizes: Vec<(&str, i64)> = db.pkgs().iter().map(|p| (p.name(), p.isize())).collect();
    sizes.sort_by_key(|(_, size)| *size);
    sizes.reverse();
    let longest = sizes
        .iter()
        .take(10)
        .map(|(p, _)| p.chars().count())
        .max()
        .unwrap_or(0);

    for (pkg, size) in sizes.into_iter().take(10) {
        println!("{:w$} {}", pkg, size.bytes(), w = longest);
    }
}

/// Display the unique groups found installed on the system.
pub(crate) fn groups(alpm: &Alpm) {
    let db = alpm.localdb();
    let mut groups = HashSet::new();

    for p in db.pkgs() {
        for g in p.groups() {
            groups.insert(g);
        }
    }

    let mut v: Vec<_> = groups.into_iter().collect();
    v.sort_unstable();

    for p in v {
        println!("{}", p);
    }
}
