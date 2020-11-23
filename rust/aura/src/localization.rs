//! Utilities for localizing messages printed by the Aura executable.

use i18n_embed::fluent::{fluent_language_loader, FluentLanguageLoader};
use i18n_embed::{DesktopLanguageRequester, I18nEmbedError, LanguageLoader};
use rust_embed::RustEmbed;
use unic_langid::LanguageIdentifier;

#[derive(RustEmbed)]
#[folder = "i18n"]
struct Translations;

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
    let mut langs: Vec<LanguageIdentifier> = lang.map(|l| vec![l]).unwrap_or_else(|| vec![]);
    langs.append(&mut DesktopLanguageRequester::requested_languages());
    let langrefs: &[&LanguageIdentifier] = &langs.iter().collect::<Vec<_>>();
    let loader = fluent_language_loader!();
    loader.load_languages(&Translations, langrefs)?;
    // loader.set_use_isolating(false);
    Ok(loader)
}
