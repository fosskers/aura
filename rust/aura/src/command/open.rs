//! Open various webpages related to Aura.

use crate::error::Error;

const BOOK_URL: &str = "https://fosskers.github.io/aura/";
const REPO_URL: &str = "https://github.com/fosskers/aura";
const BUG_URL: &str = "https://github.com/fosskers/aura/issues/new";
const AUR_URL: &str = "https://aur.archlinux.org/packages/aura";

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
    open(AUR_URL)
}

fn open(url: &str) -> Result<(), Error> {
    webbrowser::open(url)?;
    Ok(())
}
