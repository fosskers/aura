//! Download bytes from the web.

use crate::error::Error;
use curl::easy::Easy;
use linya::{Bar, Progress};
use std::io::Write;
use std::sync::{Arc, Mutex};
use std::{fs::File, path::Path};

// /// Download the contents of some URL and write them directly to a file.
// pub(crate) fn download(url: &str, target: &Path) -> Result<(), Error> {
//     download_with_progress(url, target, None)
// }

/// Same as [`download`], but shows live progress via a `ProgressBar`.
pub(crate) fn download_with_progress(
    url: &str,
    target: &Path,
    bars: Option<(Arc<Mutex<Progress>>, Bar)>,
) -> Result<(), Error> {
    // Exit early if the given target already exists.
    if target.exists() {
        Err(Error::FileConflict)?;
    }

    let mut file = File::create(target)?;
    let mut handle = Easy::new();
    handle.url(url)?;
    handle.write_function(move |bytes| file.write(bytes).or(Ok(0)))?;

    // If a progress bar was given, register it.
    if let Some((progress, bar)) = bars {
        handle.progress(true)?;
        handle.progress_function(move |_, dld, _, _| {
            let du = dld as usize;
            progress.lock().unwrap().set_and_draw(&bar, du);
            true
        })?;
    }

    handle.perform()?;

    Ok(())
}
