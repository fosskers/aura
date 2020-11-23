//! Utilities for localizing messages printed by the Aura executable.

use i18n_embed::fluent::{fluent_language_loader, FluentLanguageLoader};
use i18n_embed::{I18nEmbedError, LanguageLoader};
use rust_embed::RustEmbed;
use unic_langid::LanguageIdentifier;

#[derive(RustEmbed)]
#[folder = "i18n"]
struct Translations;

// TODO Pull `LANG`, etc., variables from the environment myself. There are
// libraries that do this, but they incur heavy dependencies.

/// Load all localizations.
///
/// ```
/// use i18n_embed_fl::fl;
///
/// let fll = aura::localization::loader(None).unwrap();
/// let msg = fl!(fll, "orphans-adopt");
/// println!("{}", msg);
/// ```
pub fn loader(lang: Option<LanguageIdentifier>) -> Result<FluentLanguageLoader, I18nEmbedError> {
    let loader = fluent_language_loader!();
    loader.load_languages(
        &Translations,
        &[lang.as_ref().unwrap_or_else(|| loader.fallback_language())],
    )?;
    loader.set_use_isolating(false);
    Ok(loader)
}

/// The list of languages that Aura has localization files for (e.g. `en-US`).
pub fn available_languages() -> Vec<String> {
    let mut vec: Vec<_> = Translations::iter().map(|l| l[0..5].to_string()).collect();
    vec.sort();
    vec
}
