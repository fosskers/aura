//! Open various webpages related to Aura.

use crate::error::Nested;
use crate::localization::Localised;
use i18n_embed_fl::fl;
use log::error;
use std::borrow::Cow;

const BOOK_URL: &str = "https://fosskers.github.io/aura/";
const REPO_URL: &str = "https://github.com/fosskers/aura";
const BUG_URL: &str = "https://github.com/fosskers/aura/issues/new";
const LIC_URL: &str = "https://github.com/fosskers/aura/blob/master/aura/LICENSE";
pub const AUR_PKG_URL: &str = "https://aur.archlinux.org/packages/";

pub(crate) enum Error {
    CouldntOpen(String, std::io::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::CouldntOpen(_, e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::CouldntOpen(url, _) => fl!(fll, "open-err", url = url.as_str()),
        }
    }
}

/// Open the Aura Book.
pub(crate) fn book() -> Result<(), Error> {
    open(BOOK_URL)
}

/// Open Aura's Github repository.
pub(crate) fn repo() -> Result<(), Error> {
    open(REPO_URL)
}

/// File a bug report for Aura.
pub(crate) fn bug() -> Result<(), Error> {
    open(BUG_URL)
}

/// Open Aura's AUR page.
pub(crate) fn aur() -> Result<(), Error> {
    let mut url = Cow::from(AUR_PKG_URL);
    url += "aura";
    open(&url)
}

/// Open Aura's LICENSE file.
pub(crate) fn license() -> Result<(), Error> {
    open(LIC_URL)
}

/// Open a given URL in a browser.
pub(crate) fn open(url: &str) -> Result<(), Error> {
    webbrowser::open(url).map_err(|e| Error::CouldntOpen(url.to_string(), e))
}
