//! Viewing the Pacman/ALPM log.

use crate::command::misc;
use crate::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::process::Command;

/// A collation of information about log entries for a particular package.
#[derive(Debug)]
struct LogEntry {
    package: String,
    /// Timestamp of the first ever installation of this package.
    installed: String,
    upgrades: usize,
    recent: Vec<String>,
}

/// Open the Pacman/ALPM log in `bat` or `less`.
pub fn view(path: &Path) -> Result<(), Error> {
    let prog = misc::viewer();
    Command::new(prog).arg(path).status().map_err(Error::IO)?;
    Ok(())
}

/// Search the Pacman log for a matching string.
pub fn search(path: &Path, term: String) -> Result<(), Error> {
    let srch = misc::searcher();
    Command::new(srch)
        .arg(term)
        .arg(path)
        .status()
        .map_err(Error::IO)?;
    Ok(())
}

/// Display install/upgrade history for the given packages.
pub fn info(path: &Path, packages: Vec<String>) {
    for e in packages.into_iter().filter_map(|p| info_single(path, p)) {
        println!("{:#?}", e);
    }
}

fn info_single(path: &Path, package: String) -> Option<LogEntry> {
    let file = File::open(path).ok()?;
    let read = BufReader::new(file);
    let patt = format!(" {} (", package);
    let hits = read
        .lines()
        .filter_map(|line| line.ok())
        .filter(|line| line.contains(&patt))
        .collect::<Vec<_>>();

    if hits.is_empty() {
        None
    } else {
        let installed = hits[0].chars().skip(1).take(16).collect();
        let upgrades = hits.iter().filter(|l| l.contains(" upgraded ")).count();
        let len = if hits.len() < 6 { 0 } else { hits.len() - 5 };
        let recent = hits.into_iter().skip(len).collect();
        let entry = LogEntry {
            package,
            installed,
            upgrades,
            recent,
        };
        Some(entry)
    }
}
