//! Utilities for localizing messages printed by the Aura executable.

use crate::utils::PathStr;
use applying::Apply;
use aura_core::aur::dependencies as deps;
use i18n_embed::fluent::fluent_language_loader;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::I18nEmbedError;
use i18n_embed::LanguageLoader;
use i18n_embed_fl::fl;
use nonempty_collections::*;
use rust_embed::RustEmbed;
use std::collections::HashMap;
use std::num::NonZeroUsize;
use unic_langid::LanguageIdentifier;

pub(crate) const TRANSLATORS: &[(&str, &str)] = &[
    ("Arabic", "\"Array in a Matrix\""),
    ("Chinese", "Kai Zhang / Alex3236"),
    ("Croatian", "Denis Kasak / \"stranac\""),
    ("Czech", "Daniel Rosel"),
    ("Dutch", "Joris Blanken / Heimen Stoffels"),
    ("Esperanto", "Zachary Matthews"),
    ("French", "Ma Jiehong / Fabien Dubosson"),
    ("German", "Lukas Niederbremer / Jonas Platte"),
    ("Hindi", "@yozachar"),
    ("Indonesia", "\"pak tua Greg\""),
    ("Italian", "Bob Valantin / Cristian Tentella"),
    ("Japanese", "Onoue Takuro / Colin Woodbury"),
    ("Korean", "\"Nioden\""),
    ("Norwegian", "\"chinatsun\""),
    ("Polish", "Chris Warrick / Michał Kurek"),
    (
        "Portuguese",
        "Henry Kupty / Thiago Perrotta / Wagner Amaral",
    ),
    ("Romanian", "90 / benone"),
    ("Russian", "Kyrylo Silin / Alexey Kotlyarov"),
    ("Serbian", "Filip Brcic"),
    ("Spanish", "Alejandro Gómez / Sergio Conde / Max Ferrer"),
    ("Swedish", "Fredrik Haikarainen / Daniel Beecham"),
    ("Turkish", "Cihan Alkan"),
    ("Ukrainian", "Andriy Cherniy"),
    ("Vietnamese", "\"Kritiqual\""),
];

#[derive(RustEmbed)]
#[folder = "i18n"]
struct Translations;

/// Parsing of [`LanguageIdentifier`]s that we are known to support.
pub(crate) fn identifier_from_locale<S>(locale: S) -> Option<LanguageIdentifier>
where
    S: AsRef<str>,
{
    match code_and_country(locale.as_ref()) {
        ("en", _) => Some(aura_pm::ENGLISH),
        ("ja", _) => Some(aura_pm::JAPANESE),
        ("pl", _) => Some(aura_pm::POLISH),
        ("hr", _) => Some(aura_pm::CROATIAN),
        ("sv", _) => Some(aura_pm::SWEDISH),
        ("de", _) => Some(aura_pm::GERMAN),
        ("es", _) => Some(aura_pm::SPANISH),
        ("pt", _) => Some(aura_pm::PORTUGUESE),
        ("fr", _) => Some(aura_pm::FRENCH),
        ("ru", _) => Some(aura_pm::RUSSIAN),
        ("it", _) => Some(aura_pm::ITALIAN),
        ("sr", _) => Some(aura_pm::SERBIAN),
        ("no", _) => Some(aura_pm::NORWEGIAN),
        ("id", _) => Some(aura_pm::INDONESIAN),
        // Mainland China.
        ("zh", Some("CN")) => Some(aura_pm::SIMPLIFIED_CHINESE),
        ("eo", _) => Some(aura_pm::ESPERANTO),
        ("nl", _) => Some(aura_pm::DUTCH),
        ("tr", _) => Some(aura_pm::TURKISH),
        ("uk", _) => Some(aura_pm::UKRAINIAN),
        ("ro", _) => Some(aura_pm::ROMANIAN),
        ("vi", _) => Some(aura_pm::VIETNAMESE),
        ("cs", _) => Some(aura_pm::CZECH),
        ("ko", _) => Some(aura_pm::KOREAN),
        ("hi", _) => Some(aura_pm::HINDI),
        _ => None,
    }
}

/// Convert from the format found in `/etc/locale.gen`, `locale -a`, or `LANG`
/// to the format parsable by us to produce [`LanguageIdentifier`]s.
pub(crate) fn code_and_country(locale: &str) -> (&str, Option<&str>) {
    let lokale = match locale.split_once('.') {
        Some((loc, _)) => loc,
        _ => locale,
    };

    match lokale.split_once(['-', '_']) {
        Some((l, c)) => (l, Some(c)),
        None => (lokale, None),
    }
}

/// Any type whose contents can be localised in a meaningful way.
pub(crate) trait Localised {
    /// Localise the content of a type.
    fn localise(&self, fll: &FluentLanguageLoader) -> String;
}

impl Localised for aura_core::git::Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            aura_core::git::Error::Io(_) => fl!(fll, "git-io"),
            aura_core::git::Error::Clone(p) => fl!(fll, "git-clone", dir = p.utf8()),
            aura_core::git::Error::Pull(p) => fl!(fll, "git-pull", dir = p.utf8()),
            aura_core::git::Error::Diff(p) => fl!(fll, "git-diff", file = p.utf8()),
            aura_core::git::Error::ReadHash(_) => fl!(fll, "git-hash"),
        }
    }
}

impl Localised for aura_core::aur::Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            aura_core::aur::Error::Git(e) => e.localise(fll),
            aura_core::aur::Error::FaurFetch(p) => fl!(fll, "faur-fetch", pkg = p.as_str()),
            aura_core::aur::Error::PackageDoesNotExist(p) => {
                fl!(fll, "faur-unknown", pkg = p.as_str())
            }
            aura_core::aur::Error::TooManyFaurResults(p) => {
                fl!(fll, "faur-too-many", pkg = p.as_str())
            }
        }
    }
}

impl<E> Localised for deps::Error<E>
where
    E: Localised,
{
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            deps::Error::PoisonedMutex => fl!(fll, "err-mutex"),
            deps::Error::R2D2(_) => fl!(fll, "err-pool-get"),
            deps::Error::Srcinfo(p, _) => fl!(fll, "err-srcinfo", file = p.utf8()),
            deps::Error::Git(e) => e.localise(fll),
            e @ deps::Error::Resolutions(_) => {
                let ne = e.inner_errors();

                if ne.len() == NonZeroUsize::MIN {
                    ne.head.localise(fll)
                } else {
                    let iter = ne.iter().map(|e| format!(" - {}", e.localise(fll)));

                    [fl!(fll, "dep-multi")]
                        .into_iter()
                        .chain(iter)
                        // FIXME Thu Jun 23 2022 Use built-in `intersperse` once it stabilizes.
                        .apply(|i| itertools::intersperse(i, "\n".to_string()))
                        .collect()
                }
            }
            deps::Error::DoesntExist(p) => fl!(fll, "dep-exist", pkg = p.as_str()),
            deps::Error::DoesntExistWithParent(a, b) => {
                fl!(fll, "dep-exist-par", pkg = a.as_str(), par = b.as_str())
            }
            deps::Error::MalformedGraph => fl!(fll, "dep-graph"),
            deps::Error::CyclicDep(cycle) => {
                let rendered: String = cycle
                    .iter()
                    .map(|s| s.as_str())
                    .apply(|iter| itertools::intersperse(iter, " => "))
                    .collect();

                fl!(fll, "dep-cycle", cycle = rendered)
            }
            deps::Error::Faur(e) => e.localise(fll),
        }
    }
}

/// Load the localizations for a particular language, or just fallback to
/// English.
///
/// ```
/// use i18n_embed_fl::fl;
///
/// let fll = aura_pm::localization::load(None).unwrap();
/// let msg = fl!(fll, "orphans-adopt");
/// println!("{}", msg);
/// ```
pub(crate) fn load(
    lang: Option<LanguageIdentifier>,
) -> Result<FluentLanguageLoader, I18nEmbedError> {
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
pub(crate) fn load_all() -> Result<HashMap<LanguageIdentifier, FluentLanguageLoader>, I18nEmbedError>
{
    available_languages()
        .into_iter()
        .map(|lang| {
            let loader = fluent_language_loader!();
            loader
                .load_languages(&Translations, &[&lang])
                .map(|_| (lang, loader))
        })
        .collect()
}

/// The list of languages that Aura has localization files for (e.g. `en-US`).
pub(crate) fn available_languages() -> Vec<LanguageIdentifier> {
    let mut vec: Vec<_> = Translations::iter()
        .filter_map(|l| l[0..5].parse().ok())
        .collect();
    vec.sort();
    vec
}

#[cfg(test)]
mod test {
    use super::*;

    /// Prove that localizations don't contain extra fields that aren't expected in
    /// English, the base language.
    #[test]
    fn no_extra_localizations() {
        let english = load(None).unwrap();
        let all = load_all().unwrap();
        for lang in available_languages() {
            all.get(&lang).unwrap().with_message_iter(&lang, |msgs| {
                for msg in msgs {
                    if !english.has(msg.id.name) {
                        panic!("{} has extra field: {}", lang, msg.id.name);
                    }
                }
            })
        }
    }

    #[test]
    fn locale_parsing() {
        assert_eq!(("en", Some("US")), code_and_country("en_US.UTF-8"));
        assert_eq!(("en", Some("US")), code_and_country("en-US.UTF-8"));
        assert_eq!(("en", None), code_and_country("en"));
    }
}
