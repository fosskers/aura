//! Log manipulation internals.

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

/// A collation of information about log entries for a particular package.
#[derive(Debug)]
pub struct LogEntry {
    /// The name of the package.
    pub package: String,
    /// Timestamp of the first ever installation of this package.
    pub installed: String,
    /// The number of upgrades since initial installation.
    pub upgrades: usize,
    /// The 5 most recent actions on this package.
    pub recent: Vec<String>,
}

/// Given a [`Path`] to the Pacman log, form some `LogEntry` statistics about a
/// particular package.
pub fn info(path: &Path, package: String) -> Option<LogEntry> {
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
