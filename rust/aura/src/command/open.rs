//! Open various webpages related to Aura.

use std::borrow::Cow;

const BOOK_URL: &str = "https://fosskers.github.io/aura/";
const REPO_URL: &str = "https://github.com/fosskers/aura";
const BUG_URL: &str = "https://github.com/fosskers/aura/issues/new";
pub const AUR_PKG_URL: &str = "https://aur.archlinux.org/packages/";

pub(crate) enum Error {
    CouldntOpen(String, std::io::Error),
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

/// Open a given URL in a browser.
pub(crate) fn open(url: &str) -> Result<(), Error> {
    webbrowser::open(url).map_err(|e| Error::CouldntOpen(url.to_string(), e))
}
