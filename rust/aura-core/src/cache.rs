//! Cache manipulation internals.

use crate::Package;
use alpm::Alpm;
use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::fs::Metadata;
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
    pub fn sudo_remove(self) -> Result<(), PathBuf> {
        match Command::new("sudo").arg("rm").arg(&self.path).status() {
            Ok(s) if s.success() => Ok(()),
            Ok(_) | Err(_) => Err(self.path),
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

/// All package tarball filenames that match a given string.
pub fn search<'a, P>(caches: &'a [P], term: &'a str) -> impl Iterator<Item = PathBuf> + 'a
where
    P: AsRef<Path>,
{
    crate::read_dirs(caches)
        .filter_map(|r| r.ok())
        .map(|de| de.path())
        .filter(move |path| {
            path.file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.contains(term))
                .unwrap_or(false)
        })
}

/// Yield the [`CacheInfo`], if possible, of the given packages.
pub fn info(caches: &[&Path], package: &str) -> Result<Option<CacheInfo>, std::io::Error> {
    let mut matches: Vec<(PkgPath, Metadata)> = search(caches, package)
        .filter_map(|path| {
            path.metadata()
                .ok()
                .and_then(|meta| PkgPath::new(path).map(|pp| (pp, meta)))
        })
        .filter(|(pp, _)| pp.pkg.name == package)
        .collect();
    matches.sort_by(|(p0, _), (p1, _)| p1.cmp(p0));

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

/// The number of files and all bytes consumed by files contained in the given
/// directory `Path`s.
pub fn size<P>(paths: &[P]) -> CacheSize
where
    P: AsRef<Path>,
{
    let (files, bytes) = crate::read_dirs(paths)
        .filter_map(|de| de.ok())
        .filter(|de| is_package(&de.path()))
        .filter_map(|de| de.metadata().ok())
        .map(|meta| meta.len())
        .fold((0, 0), |(ac, al), l| (ac + 1, al + l));

    CacheSize { files, bytes }
}

/// Valid [`PkgPath`]s in the given caches.
pub fn package_paths<P>(caches: &[P]) -> impl Iterator<Item = PkgPath<'static>> + '_
where
    P: AsRef<Path>,
{
    crate::read_dirs(caches)
        .filter_map(|r| r.ok())
        .map(|de| de.path())
        .filter_map(PkgPath::new)
}

/// Installed official packages that have no tarball in the cache.
pub fn officials_missing_tarballs<'a>(
    alpm: &'a Alpm,
    caches: &[&Path],
) -> impl Iterator<Item = alpm::Package<'a>> {
    let groups = all_versions(caches);

    aura_arch::officials(alpm).filter(move |p| {
        let pv = p.version().as_str();
        groups
            .get(p.name())
            .map(|vs| !vs.contains(pv))
            .unwrap_or(true)
    })
}

/// Installed foreign packages that have no tarball in the cache.
pub fn foreigns_missing_tarballs<'a>(
    alpm: &'a Alpm,
    caches: &[&Path],
) -> impl Iterator<Item = alpm::Package<'a>> {
    let groups = all_versions(caches);

    aura_arch::foreigns(alpm).filter(move |p| {
        let pv = p.version().as_str();
        groups
            .get(p.name())
            .map(|vs| !vs.contains(pv))
            .unwrap_or(true)
    })
}

/// Installed packages that have no tarball in the cache.
pub fn missing_tarballs<'a>(
    alpm: &'a Alpm,
    caches: &[&Path],
) -> impl Iterator<Item = alpm::Package<'a>> {
    let groups: HashMap<String, HashSet<String>> = all_versions(caches);

    alpm.localdb().pkgs().into_iter().filter(move |p| {
        let pv = p.version().as_str();
        groups
            .get(p.name())
            .map(|vs| !vs.contains(pv))
            .unwrap_or(true)
    })
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
    path.to_str()
        .map(|p| p.ends_with(".pkg.tar.zst") || p.ends_with(".pkg.tar.xz"))
        .unwrap_or(false)
}

/// Every version of every package available in the caches.
pub fn all_versions(caches: &[&Path]) -> HashMap<String, HashSet<String>> {
    let mut map = HashMap::new();

    for pp in package_paths(caches) {
        let pkg = pp.as_package();
        let set = map.entry(pkg.name.to_string()).or_insert_with(HashSet::new);
        set.insert(pkg.version.to_string());
    }

    map
}
