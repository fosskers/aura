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

// TODO
// pl-PL Polish
// hr-HR Crotian
// sv-SE Swedish
// de-DE German
// es-ES Spanish
// pt-PT Portuguese
// fr-FR French
// ru-RU Russian
// it-IT Italian
// sr-SP Serbian
// nb-NO Norwegian
// id-ID Indonesian
// zh-CN Chinese
// nl-NL Dutch
// ??? Esperanto ???

/// Load the localizations for a particular language, or just fallback to
/// English.
///
/// ```
/// use i18n_embed_fl::fl;
///
/// let fll = aura::localization::load(None).unwrap();
/// let msg = fl!(fll, "orphans-adopt");
/// println!("{}", msg);
/// ```
pub fn load(lang: Option<LanguageIdentifier>) -> Result<FluentLanguageLoader, I18nEmbedError> {
    let loader = fluent_language_loader!();
    loader.load_languages(
        &Translations,
        &[lang.as_ref().unwrap_or_else(|| loader.fallback_language())],
    )?;
    loader.set_use_isolating(false);
    Ok(loader)
}

/// Like [`load`], but loads all available languages.
///
/// There is no guarantee about what language will end up the default, so this
/// shouldn't be used for normal localization purposes.
pub fn load_all() -> Result<FluentLanguageLoader, I18nEmbedError> {
    let loader = fluent_language_loader!();
    let langs = available_languages();
    let slice: Vec<&LanguageIdentifier> = langs.iter().collect();
    loader.load_languages(&Translations, &slice)?;
    loader.set_use_isolating(false);
    Ok(loader)
}

/// The list of languages that Aura has localization files for (e.g. `en-US`).
pub fn available_languages() -> Vec<LanguageIdentifier> {
    let mut vec: Vec<_> = Translations::iter()
        .filter_map(|l| l[0..5].parse().ok())
        .collect();
    vec.sort();
    vec
}
