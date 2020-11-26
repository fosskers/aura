//! Cache manipulation internals.

use std::fs::ReadDir;
use std::path::{Path, PathBuf};

/// An iterator of filepaths that matched some search term.
pub struct CacheMatches {
    read_dir: ReadDir,
    term: String,
}

impl Iterator for CacheMatches {
    type Item = PathBuf;

    fn next(&mut self) -> Option<PathBuf> {
        match self.read_dir.next() {
            None => None,
            Some(Err(_)) => self.next(),
            Some(Ok(entry)) => {
                let path = entry.path();
                match path.to_str() {
                    Some(s) if s.contains(&self.term) => Some(path),
                    _ => self.next(),
                }
            }
        }
    }
}

/// All package filenames that match a given string.
pub fn search(path: &Path, term: String) -> Result<CacheMatches, std::io::Error> {
    let read_dir = path.read_dir()?;
    Ok(CacheMatches { read_dir, term })
}

/// The number of files and all bytes consumed by a given `Path`.
pub fn size(path: &Path) -> Result<(u64, u64), std::io::Error> {
    let res = path
        .read_dir()?
        .filter_map(|de| de.ok())
        .filter_map(|de| de.metadata().ok())
        .map(|meta| meta.len())
        .fold((0, 0), |(ac, al), l| (ac + 1, al + l));
    Ok(res)
}
