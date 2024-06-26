//! Download bytes from the web.

use curl::easy::Easy;
use linya::{Bar, Progress};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};

// #[derive(FromVariants)]
// pub(crate) enum Error {
//     Io(std::io::Error),
//     Curl(curl::Error),
// }

// /// Download the contents of some URL and write them directly to a file.
// pub(crate) fn download(url: &str, target: &Path) -> Result<(), Error> {
//     download_with_progress(url, target, None)
// }

/// Same as [`download`], but shows live progress via a `ProgressBar`.
pub(crate) fn download_with_progress(
    url: &str,
    target: &Path,
    bars: Option<(Arc<Mutex<Progress>>, &Bar)>,
) -> Option<()> {
    // Overwrites the file if it already exists.
    let mut file = File::create(target).ok()?;
    let mut handle = Easy::new();
    handle.url(url).ok()?;
    handle.progress(true).ok()?;
    handle.fail_on_error(true).ok()?;

    // A separate scoped `Transfer` lets us specify callbacks like
    // `write_function` and `progress_function` with relaxed lifetimes.
    let mut tx = handle.transfer();
    tx.write_function(move |bytes| file.write(bytes).or(Ok(0)))
        .ok()?;

    // If a progress bar was given, register it.
    if let Some((progress, bar)) = bars {
        tx.progress_function(move |_, dld, _, _| {
            let du = dld as usize;
            progress.lock().unwrap().set_and_draw(bar, du);
            true
        })
        .ok()?;
    }

    tx.perform().ok()
}
