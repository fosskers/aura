//! Types and modules that need to be shared across components.

pub mod flags;

use std::str::FromStr;

use unic_langid::{langid, LanguageIdentifier};

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
