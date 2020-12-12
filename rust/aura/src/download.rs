//! Download bytes from the web.

use crate::error::Error;
use curl::easy::Easy;
use std::io::Write;
use std::{fs::File, path::Path};

/// Download the contents of some URL and write them directly to a file.
pub(crate) fn download(url: &str, target: &Path) -> Result<(), Error> {
    // Exit early if the given target already exists.
    if target.exists() {
        Err(Error::FileConflict)?;
    }

    let mut file = File::create(target)?;
    let mut handle = Easy::new();
    handle.url(url)?;
    handle.write_function(move |bytes| file.write(bytes).or(Ok(0)))?;
    handle.perform()?;

    Ok(())
}
