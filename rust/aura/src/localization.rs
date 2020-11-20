//! Utilities for localizing messages printed by the Aura executable.

use i18n_embed::fluent::{fluent_language_loader, FluentLanguageLoader};
use i18n_embed::{I18nEmbedError, LanguageLoader};
use rust_embed::RustEmbed;
use unic_langid::LanguageIdentifier;

#[derive(RustEmbed)]
#[folder = "i18n"]
struct Translations;

pub fn loader(lang: Option<LanguageIdentifier>) -> Result<FluentLanguageLoader, I18nEmbedError> {
    let loader = fluent_language_loader!();
    loader.load_languages(
        &Translations,
        &[lang.as_ref().unwrap_or_else(|| loader.fallback_language())],
    )?;
    Ok(loader)
}
