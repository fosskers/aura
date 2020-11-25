//! Statistics about the user's machine or about Aura itself.

use crate::error::Error;
use crate::localization;
use alpm::Alpm;
use i18n_embed::LanguageLoader;
use std::collections::HashMap;
use ubyte::ToByteUnit;
use unic_langid::LanguageIdentifier;

/// Raw contents of loaded localizations.
pub fn localization() -> Result<(), Error> {
    let fll = localization::load_all().map_err(Error::I18n)?;
    let stats: HashMap<LanguageIdentifier, usize> = localization::available_languages()
        .into_iter()
        .map(|lang| {
            let count = fll.with_message_iter(&lang, |iter| iter.count());
            (lang, count)
        })
        .collect();

    let english = fll.fallback_language();
    let max = stats.get(&english).unwrap().clone();

    for (lang, count) in stats {
        let perc = 100.0 * count as f64 / max as f64;
        println!("{} {}/{} ({:.2}%)", lang, count, max, perc);
    }

    Ok(())
}

/// Display the Top 10 packages with the biggest installation footprint.
pub fn heavy_packages(alpm: &Alpm) {
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
