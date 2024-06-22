//! Statistics about the user's machine or about Aura itself.

use crate::env::Env;
use crate::error::Nested;
use crate::localization::{self, Localised};
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use r2d2_alpm::Alpm;
use std::collections::{HashMap, HashSet};
use std::io::BufWriter;
use ubyte::ToByteUnit;
use unic_langid::{langid, LanguageIdentifier};

#[derive(FromVariants)]
pub(crate) enum Error {
    LangLoad(i18n_embed::I18nEmbedError),
    Env(crate::env::Error),
    Stdout,
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::LangLoad(e) => error!("{e}"),
            Error::Env(e) => e.nested(),
            Error::Stdout => {}
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::LangLoad(_) => fl!(fll, "stats-local"),
            Error::Env(e) => e.localise(fll),
            Error::Stdout => fl!(fll, "err-write"),
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
        "ja" | "zh" => raw * 2,
        _ => raw,
    }
}

/// Basic stats about the current machine.
pub(crate) fn stats(env: &Env, fll: &FluentLanguageLoader) -> Result<(), Error> {
    let alpm = env.alpm()?;
    let mut w = BufWriter::new(std::io::stdout());

    let pkgs = installed_packages(&alpm);
    let aura_cache_bytes = aura_core::recursive_dir_size(&env.aur.cache);
    let pacman_cache_bytes: u64 = env
        .pacman
        .cache_dir
        .iter()
        .map(aura_core::recursive_dir_size)
        .sum();
    let aura_build_bytes = aura_core::recursive_dir_size(&env.aur.build);
    let tmp_bytes = aura_core::recursive_dir_size("/tmp");

    let pairs = vec![
        (
            fl!(fll, "stats-host"),
            whoami::fallible::hostname()
                .unwrap_or_else(|_| "Unknown".to_string())
                .normal(),
        ),
        (fl!(fll, "stats-user"), whoami::username().normal()),
        (fl!(fll, "stats-distro"), whoami::distro().normal()),
        (fl!(fll, "stats-editor"), env.general.editor.normal()),
        (fl!(fll, "stats-pkgs"), pkgs.to_string().normal()),
        (
            fl!(fll, "stats-pacman-cache"),
            format!("{}", pacman_cache_bytes.bytes()).normal(),
        ),
        (
            fl!(fll, "stats-aura-cache"),
            format!("{}", aura_cache_bytes.bytes()).normal(),
        ),
        (
            fl!(fll, "stats-aura-build"),
            format!("{}", aura_build_bytes.bytes()).normal(),
        ),
        (
            fl!(fll, "stats-tmp"),
            format!("{}", tmp_bytes.bytes()).normal(),
        ),
    ];

    crate::utils::info(&mut w, fll.current_language(), &pairs).map_err(|_| Error::Stdout)
}

/// The number of packages installed on the system.
fn installed_packages(alpm: &Alpm) -> usize {
    alpm.as_ref().localdb().pkgs().iter().count()
}

/// Display the Top 10 packages with the biggest installation footprint.
pub(crate) fn heavy_packages(alpm: &Alpm) {
    let db = alpm.as_ref().localdb();
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
    let db = alpm.as_ref().localdb();
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
