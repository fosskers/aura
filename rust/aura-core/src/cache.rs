//! Cache manipulation internals.

use std::path::Path;
use std::{
    fs::{DirEntry, ReadDir},
    time::SystemTime,
};

/// A description of the size of the package cache.
pub struct CacheSize {
    /// The number of package files in the cache.
    pub files: u64,
    /// The number of bytes of all files combined.
    pub bytes: u64,
}

/// An iterator of filepaths that matched some search term.
pub struct CacheMatches<'a> {
    read_dir: ReadDir,
    term: &'a str,
}

impl<'a> Iterator for CacheMatches<'a> {
    type Item = DirEntry;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_dir.next() {
            None => None,
            Some(Err(_)) => self.next(),
            Some(Ok(entry)) => match entry.path().to_str() {
                Some(s) if s.contains(self.term) => Some(entry),
                _ => self.next(),
            },
        }
    }
}

/// Cache statistics for a particular package.
#[derive(Debug)]
pub struct CacheInfo {
    /// The name of the package.
    pub name: String,
    /// The most recent version of the package available.
    pub version: String,
    /// The date/time that the tarball was downloaded or built.
    pub created: SystemTime,
    /// Is a signature file present for this entry?
    pub signature: bool,
    /// Size in bytes of the tarball.
    pub size: u64,
}

/// All package filenames that match a given string.
pub fn search<'a>(cache: &Path, term: &'a str) -> Result<CacheMatches<'a>, std::io::Error> {
    let read_dir = cache.read_dir()?;
    Ok(CacheMatches { read_dir, term })
}

/// Yield the [`CacheInfo`], if possible, of the given packages.
pub fn info(cache: &Path, package: &str) -> Result<Option<CacheInfo>, std::io::Error> {
    let mut matches: Vec<_> = search(cache, package)?
        .filter(|de| is_package(&de.path()))
        .filter_map(|de| split_path(&de.path()).map(|(name, ver)| (name, ver, de)))
        .filter(|(name, _, _)| name == package)
        .collect();
    matches.sort_by(|(_, v0, _), (_, v1, _)| alpm::vercmp(v1.as_str(), v0.as_str()));

    match matches.into_iter().next() {
        None => Ok(None),
        Some((name, version, de)) => {
            let meta = de.metadata()?;
            let created = meta.created()?;
            let mut path = de.path();

            match sig_extension(&path) {
                None => Ok(None),
                Some(ext) => {
                    path.set_extension(ext);
                    let info = CacheInfo {
                        name,
                        version,
                        created,
                        signature: path.exists(),
                        size: meta.len(),
                    };

                    Ok(Some(info))
                }
            }
        }
    }
}

/// The number of files and all bytes consumed by a given `Path`.
pub fn size(path: &Path) -> Result<CacheSize, std::io::Error> {
    let (files, bytes) = path
        .read_dir()?
        .filter_map(|de| de.ok())
        .filter(|de| is_package(&de.path()))
        .filter_map(|de| de.metadata().ok())
        .map(|meta| meta.len())
        .fold((0, 0), |(ac, al), l| (ac + 1, al + l));
    Ok(CacheSize { files, bytes })
}

// TODO Provide a similar function for signature files.
/// Is a given `Path` a legal Arch Linux package tarball?
///
/// ```
/// use aura_core::cache::is_package;
/// use std::path::Path;
///
/// assert!(is_package(Path::new("libebml-1.3.10-1-x86_64.pkg.tar.xz")));
/// assert!(is_package(Path::new("libebml-1.4.0-1-x86_64.pkg.tar.zst")));
/// ```
pub fn is_package(path: &Path) -> bool {
    match path.to_str() {
        None => false,
        Some(s) => s.ends_with(".pkg.tar.zst") || s.ends_with(".pkg.tar.xz"),
    }
}

// TODO Avoid the extra String allocation.
/// Split a path into its package name and version.
///
/// ```
/// use aura_core::cache;
/// use std::path::Path;
///
/// let path = Path::new("/var/cache/pacman/pkg/aura-bin-3.2.1-1-x86_64.pkg.tar.zst");
/// assert_eq!(Some(("aura-bin".to_string(), "3.2.1-1".to_string())), cache::split_path(path));
/// ```
pub fn split_path(path: &Path) -> Option<(String, String)> {
    path.file_name()
        .and_then(|file| file.to_str())
        .and_then(|file| file.rsplitn(2, '-').skip(1).next())
        .and_then(|pkg| {
            let mut vec: Vec<_> = pkg.rsplitn(3, '-').collect();
            let name = vec.last()?.to_string();
            vec.pop();
            vec.reverse();
            let ver = vec.join("-");

            Some((name, ver))
        })
}

/// Feed the output of this to [`PathBuf::set_extension`].
fn sig_extension(path: &Path) -> Option<String> {
    path.extension()
        .and_then(|s| s.to_str())
        .map(|ext| vec![ext, "sig"].join("."))
}
