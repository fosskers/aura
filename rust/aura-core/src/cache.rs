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
