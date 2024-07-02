//! Types and modules that need to be shared across components.

pub mod flags;

use std::str::FromStr;
use unic_langid::langid;
use unic_langid::LanguageIdentifier;

/// A wrapper around [`time::Date`] to supply some trait instances.
#[derive(Debug, Clone)]
pub struct Date(pub time::Date);

impl FromStr for Date {
    type Err = time::error::Parse;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        time::Date::parse(
            s,
            &time::macros::format_description!("[year]-[month]-[day]"),
        )
        .map(Date)
    }
}

pub const CROATIAN: LanguageIdentifier = langid!("hr-HR");
pub const ENGLISH: LanguageIdentifier = langid!("en-US");
pub const GERMAN: LanguageIdentifier = langid!("de-DE");
pub const JAPANESE: LanguageIdentifier = langid!("ja-JP");
pub const POLISH: LanguageIdentifier = langid!("pl-PL");
pub const SWEDISH: LanguageIdentifier = langid!("sv-SE");
pub const SPANISH: LanguageIdentifier = langid!("es-ES");
pub const PORTUGUESE: LanguageIdentifier = langid!("pt-PT");
pub const FRENCH: LanguageIdentifier = langid!("fr-FR");
pub const RUSSIAN: LanguageIdentifier = langid!("ru-RU");
pub const ITALIAN: LanguageIdentifier = langid!("it-IT");
pub const SERBIAN: LanguageIdentifier = langid!("sr-RS");
pub const NORWEGIAN: LanguageIdentifier = langid!("no-NO");
pub const INDONESIAN: LanguageIdentifier = langid!("id-ID");
pub const SIMPLIFIED_CHINESE: LanguageIdentifier = langid!("zh-CN");
pub const ESPERANTO: LanguageIdentifier = langid!("eo-EO");
pub const DUTCH: LanguageIdentifier = langid!("nl-NL");
pub const TURKISH: LanguageIdentifier = langid!("tr-TR");
pub const ARABIC: LanguageIdentifier = langid!("ar-SA");
pub const UKRAINIAN: LanguageIdentifier = langid!("uk-UA");
pub const ROMANIAN: LanguageIdentifier = langid!("ro-RO");
pub const VIETNAMESE: LanguageIdentifier = langid!("vi-VN");
pub const CZECH: LanguageIdentifier = langid!("cs-CZ");
pub const KOREAN: LanguageIdentifier = langid!("ko-KR");
pub const HINDI: LanguageIdentifier = langid!("hi-IN");
