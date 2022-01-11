//! Cache manipulation internals.

use crate::common::Package;
use alpm::Alpm;
use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::fs::{Metadata, ReadDir};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::{cmp::Ordering, process::Command};

/// A validated path to a package tarball.
#[derive(Debug, PartialEq, Eq)]
pub struct PkgPath<'a> {
    path: PathBuf,
    pkg: Package<'a>,
}

impl<'a> PkgPath<'a> {
    /// Validate that `PathBuf` has an expected extension.
    pub fn new(path: PathBuf) -> Option<PkgPath<'static>> {
        match Package::from_path(&path) {
            Some(pkg) if is_package(&path) => Some(PkgPath { path, pkg }),
            _ => None,
        }
    }

    /// The path postfixed by its `.sig` extension.
    pub fn sig_file(&self) -> PathBuf {
        let mut new: PathBuf = self.path.clone();
        let mut ext: OsString = new.extension().unwrap().to_os_string();
        ext.push(".sig");
        new.set_extension(ext);

        new
    }

    /// The internal `Path` of this validated tarball.
    pub fn as_path(&self) -> &Path {
        &self.path
    }

    /// Consume this `PkgPath` to get its inner `PathBuf`.
    pub fn into_pathbuf(self) -> PathBuf {
        self.path
    }

    /// Pull a simple package definition from this tarball path.
    pub fn as_package(&self) -> &Package<'a> {
        &self.pkg
    }

    /// Delete this `PkgPath` and its `.sig` file, if there is one.
    pub fn remove(self) -> Result<(), std::io::Error> {
        std::fs::remove_file(&self.path)?;

        let sig = self.sig_file();
        if sig.exists() {
            std::fs::remove_file(sig)?;
        }

        Ok(())
    }

    // TODO I'd like it if this could be avoided.
    /// Remove this via a shell call to `rm`.
    pub fn sudo_remove(self) -> Option<()> {
        match Command::new("sudo").arg("rm").arg(self.path).status() {
            Ok(es) if es.success() => Some(()),
            _ => None,
        }
    }
}

impl<'a> PartialOrd for PkgPath<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for PkgPath<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.pkg.cmp(&other.pkg)
    }
}

/// A description of the size of the package cache.
pub struct CacheSize {
    /// The number of package files in the cache.
    pub files: usize,
    /// The number of bytes of all files combined.
    pub bytes: u64,
}

/// An iterator of filepaths that matched some search term.
pub struct CacheMatches<'a> {
    read_dir: ReadDir,
    term: &'a str,
}

impl<'a> Iterator for CacheMatches<'a> {
    type Item = PathBuf;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_dir.next()? {
            Err(_) => self.next(),
            Ok(entry) => {
                let path = entry.path();
                match path.to_str() {
                    Some(s) if s.contains(self.term) => Some(path),
                    _ => self.next(),
                }
            }
        }
    }
}

/// An iterator of all legal package tarballs in the cache.
pub struct PkgPaths {
    read_dir: ReadDir,
}

impl Iterator for PkgPaths {
    type Item = PkgPath<'static>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_dir.next()? {
            Err(_) => self.next(),
            Ok(entry) => PkgPath::new(entry.path()).or_else(|| self.next()),
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
    /// Available versions.
    pub available: Vec<String>,
}

/// All package filenames that match a given string.
pub fn search<'a>(cache: &Path, term: &'a str) -> Result<CacheMatches<'a>, std::io::Error> {
    let read_dir = cache.read_dir()?;
    Ok(CacheMatches { read_dir, term })
}

/// Yield the [`CacheInfo`], if possible, of the given packages.
pub fn info(cache: &Path, package: &str) -> Result<Option<CacheInfo>, std::io::Error> {
    let mut matches: Vec<(PkgPath, Metadata)> = search(cache, package)?
        // FIXME Mon Jan 10 19:23:07 2022
        //
        // Use two `filter_map` steps instead of one in order to avoid the
        // nesting.
        .filter_map(|path| {
            path.metadata()
                .ok()
                .and_then(|meta| PkgPath::new(path).map(|pp| (pp, meta)))
        })
        .filter(|(pp, _)| pp.pkg.name == package)
        .collect();
    matches.sort_by(|(p0, _), (p1, _)| p1.cmp(&p0));

    let available: Vec<String> = matches
        .iter()
        .map(|(pp, _)| pp.pkg.version.to_string())
        .collect();

    match matches.into_iter().next() {
        None => Ok(None),
        Some((pp, meta)) => {
            let created = meta.created()?;
            let signature = pp.sig_file().exists();

            let info = CacheInfo {
                name: pp.pkg.name.into_owned(),
                version: pp.pkg.version.into_owned(),
                created,
                signature,
                size: meta.len(),
                available,
            };

            Ok(Some(info))
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

/// All valid [`PkgPath`]s in the given cache.
pub fn package_paths(path: &Path) -> Result<PkgPaths, std::io::Error> {
    let read_dir = path.read_dir()?;
    Ok(PkgPaths { read_dir })
}

/// Installed official packages that have no tarball in the cache.
pub fn officials_missing_tarballs<'a>(
    alpm: &'a Alpm,
    path: &Path,
) -> Result<impl Iterator<Item = alpm::Package<'a>>, std::io::Error> {
    let groups = all_versions(path)?;

    let missings = aura_arch::officials(alpm).filter(move |p| {
        let pv = p.version().as_str();
        groups
            .get(p.name())
            .map(|vs| !vs.contains(pv))
            .unwrap_or(true)
    });

    Ok(missings)
}

/// Installed packages that have no tarball in the cache.
pub fn missing_tarballs<'a>(
    alpm: &'a Alpm,
    path: &Path,
) -> Result<impl Iterator<Item = alpm::Package<'a>>, std::io::Error> {
    let groups = all_versions(path)?;

    let missings = alpm.localdb().pkgs().into_iter().filter(move |p| {
        let pv = p.version().as_str();
        groups
            .get(p.name())
            .map(|vs| !vs.contains(pv))
            .unwrap_or(true)
    });

    Ok(missings)
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

/// Every version of every package available in the cache.
pub fn all_versions(cache: &Path) -> Result<HashMap<String, HashSet<String>>, std::io::Error> {
    let mut map = HashMap::new();

    for pp in package_paths(cache)? {
        let pkg = pp.as_package();
        let set = map.entry(pkg.name.to_string()).or_insert_with(HashSet::new);
        set.insert(pkg.version.to_string());
    }

    Ok(map)
}
