//! All functionality involving the `-A` command.

use crate::error::Error;
use std::borrow::Cow;

/// Open a given package's AUR package.
pub(crate) fn open(package: &str) -> Result<(), Error> {
    let mut url = Cow::from(crate::open::AUR_URL);
    url += package;
    crate::open::open(&url)
}
