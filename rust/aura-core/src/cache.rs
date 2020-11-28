//! Cache manipulation internals.

use std::fs::ReadDir;
use std::path::{Path, PathBuf};

/// A description of the size of the package cache.
pub struct CacheSize {
    /// The number of package files in the cache.
    pub files: u64,
    /// The number of bytes of all files combined.
    pub bytes: u64,
}

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

// TODO Filter on legal package extensions!
/// The number of files and all bytes consumed by a given `Path`.
pub fn size(path: &Path) -> Result<CacheSize, std::io::Error> {
    let (files, bytes) = path
        .read_dir()?
        .filter_map(|de| de.ok())
        .filter_map(|de| de.metadata().ok())
        .map(|meta| meta.len())
        .fold((0, 0), |(ac, al), l| (ac + 1, al + l));
    Ok(CacheSize { files, bytes })
}
