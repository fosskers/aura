//! Download bytes from the web.

use crate::error::Error;
use curl::easy::Easy;
use pbr::ProgressBar;
use std::io::Write;
use std::{fs::File, path::Path};

/// Download the contents of some URL and write them directly to a file.
pub(crate) fn download(url: &str, target: &Path) -> Result<(), Error> {
    download_with_progress::<std::io::Stdout>(url, target, None)
}

/// Same as [`download`], but shows live progress via a `ProgressBar`.
pub(crate) fn download_with_progress<T: 'static + Write + Send>(
    url: &str,
    target: &Path,
    bar: Option<ProgressBar<T>>,
) -> Result<(), Error> {
    // Exit early if the given target already exists.
    if target.exists() {
        Err(Error::FileConflict)?;
    }

    let mut file = File::create(target)?;
    let mut handle = Easy::new();
    handle.url(url)?;
    handle.progress(true)?;
    handle.write_function(move |bytes| file.write(bytes).or(Ok(0)))?;

    if let Some(mut b) = bar {
        handle.progress_function(move |_, dld, _, _| {
            let du = dld as u64;
            b.set(du);
            true
        })?;
    }

    handle.perform()?;

    Ok(())
}
