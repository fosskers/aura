//! Statistics about the user's machine or about Aura itself.

use crate::error::Error;
use crate::localization;
use i18n_embed::LanguageLoader;
use std::collections::HashMap;
use unic_langid::LanguageIdentifier;

/// Raw contents of loaded localizations.
pub fn localization() -> Result<(), Error> {
    let fll = localization::loader_all().map_err(Error::I18n)?;
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
