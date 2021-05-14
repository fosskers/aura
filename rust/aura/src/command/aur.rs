//! All functionality involving the `-A` command.

use crate::error::Error;
use raur_curl::{Handle, Raur};
use std::borrow::Cow;

/// View AUR package information.
pub(crate) fn info(packages: &[String]) -> Result<(), Error> {
    let h = Handle::new();
    let r = h.info(packages)?;

    for p in r {
        println!("{}-{}", p.name, p.version);
    }

    Ok(())
}

/// Open a given package's AUR package.
pub(crate) fn open(package: &str) -> Result<(), Error> {
    let mut url = Cow::from(crate::open::AUR_URL);
    url += package;
    crate::open::open(&url)
}
