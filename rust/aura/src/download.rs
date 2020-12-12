//! Download bytes from the web.

use crate::error::Error;
use curl::easy::Easy;
use pbr::{MultiBar, ProgressBar};
use rayon::prelude::*;
use std::io::Write;
use std::{fs::File, path::Path, time::Duration};

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

            // TODO Check if this can be removed.
            if du >= b.total {
                b.finish();
            }

            true
        })?;
    }

    handle.perform()?;

    Ok(())
}

// pub(crate) fn bar_test() {
//     let mb = Arc::new(MultiBar::new());
//     std::thread::spawn({
//         let mb = Arc::clone(&mb);
//         move || {
//             mb.listen();
//         }
//     });

//     (0..30).into_par_iter().for_each(|n| {
//         let mut bar = mb.create_bar(100);
//         bar.message(&format!("{} ", n));

//         for _ in 0..100 {
//             bar.inc();
//             std::thread::sleep(Duration::from_millis(50));
//         }

//         bar.finish_print(&format!("DONE: {}", n));
//     });

//     mb.listen();
// }

pub(crate) fn bar_test2() {
    let mb = MultiBar::new();
    let foos: Vec<_> = (0..30).map(|n| (n, mb.create_bar(100))).collect();

    std::thread::spawn(move || {
        mb.listen();
    });

    foos.into_par_iter().for_each(|(n, mut bar)| {
        bar.message(&format!("{} ", n));
        for _ in 0..100 {
            bar.inc();
            std::thread::sleep(Duration::from_millis(50));
        }
        bar.finish();
    })
}
