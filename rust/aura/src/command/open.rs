//! Open various webpages related to Aura.

use anyhow::{Context, Result};

const BOOK_URL: &str = "https://fosskers.github.io/aura/";
const REPO_URL: &str = "https://github.com/fosskers/aura";
const BUG_URL: &str = "https://github.com/fosskers/aura/issues/new";
const AUR_URL: &str = "https://aur.archlinux.org/packages/aura";

/// Open the Aura Book.
pub fn book() -> Result<()> {
    open(BOOK_URL)
}

/// Open Aura's Github repository.
pub fn repo() -> Result<()> {
    open(REPO_URL)
}

/// File a bug report for Aura.
pub fn bug() -> Result<()> {
    open(BUG_URL)
}

/// Open Aura's AUR page.
pub fn aur() -> Result<()> {
    open(AUR_URL)
}

fn open(url: &str) -> Result<()> {
    webbrowser::open(url).context("failed to open browser")?;
    Ok(())
}
