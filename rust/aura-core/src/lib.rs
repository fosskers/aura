//! Core package manager functionality that doesn't assume a certain frontend,
//! logging framework, or Error stack.

#![warn(missing_docs)]

pub mod aur;
pub mod cache;
pub mod deps;
pub mod faur;
pub mod git;
pub mod log;
pub mod snapshot;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::fs::DirEntry;
use std::path::Path;

/// The simplest form a package.
#[derive(Debug, PartialEq, Eq)]
pub struct Package<'a> {
    /// The name of the package.
    pub name: Cow<'a, str>,
    /// The version of the package.
    pub version: Cow<'a, str>,
}

impl<'a> Package<'a> {
    /// Construct a new `Package`.
    pub fn new<S, T>(name: S, version: T) -> Package<'a>
    where
        S: Into<Cow<'a, str>>,
        T: Into<Cow<'a, str>>,
    {
        Package {
            name: name.into(),
            version: version.into(),
        }
    }

    // TODO Avoid the extra String allocation.
    /// Split a [`Path`] into its package name and version.
    ///
    /// ```
    /// use aura_core::Package;
    /// use std::path::Path;
    ///
    /// let path = Path::new("/var/cache/pacman/pkg/aura-bin-3.2.1-1-x86_64.pkg.tar.zst");
    /// let pkg = Package::from_path(path).unwrap();
    /// assert_eq!("aura-bin", pkg.name);
    /// assert_eq!("3.2.1-1", pkg.version);
    ///
    /// let simple = Path::new("aura-bin-3.2.1-1-x86_64.pkg.tar.zst");
    /// let pkg = Package::from_path(simple).unwrap();
    /// assert_eq!("aura-bin", pkg.name);
    /// assert_eq!("3.2.1-1", pkg.version);
    /// ```
    pub fn from_path(path: &Path) -> Option<Package<'static>> {
        path.file_name()
            .and_then(|file| file.to_str())
            // FIXME Mon Jan 10 19:19:47 2022
            //
            // Consider `rsplit_once` etc. here.
            .and_then(|file| file.rsplit_once('-'))
            .and_then(|(pkg, _)| {
                let mut vec: Vec<_> = pkg.rsplitn(3, '-').collect();
                let name = vec.last()?.to_string();
                vec.pop();
                vec.reverse();
                let version = vec.join("-");

                Some(Package::new(name, version))
            })
    }

    /// Does some given version string have the same value as the one in this
    /// `Package`?
    pub fn same_version(&self, other: &str) -> bool {
        match alpm::vercmp(other, &self.version) {
            Ordering::Equal => true,
            Ordering::Less | Ordering::Greater => false,
        }
    }
}

impl<'a> From<alpm::Package<'a>> for Package<'a> {
    fn from(p: alpm::Package<'a>) -> Self {
        Package::new(p.name(), p.version().as_str())
    }
}

impl From<crate::faur::Package> for Package<'_> {
    fn from(p: crate::faur::Package) -> Self {
        Package::new(p.name, p.version)
    }
}

impl<'a> PartialOrd for Package<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Package<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.name.cmp(&other.name) {
            Ordering::Equal => alpm::vercmp(self.version.as_ref(), other.version.as_ref()),
            otherwise => otherwise,
        }
    }
}

/// Like [`Path::read_dir`], but for multiple [`Path`]s at once.
pub fn read_dirs<P>(paths: &[P]) -> impl Iterator<Item = Result<DirEntry, std::io::Error>> + '_
where
    P: AsRef<Path>,
{
    paths
        .iter()
        .filter_map(|path| path.as_ref().read_dir().ok())
        .flatten()
}

/// Apply functions in method-position.
pub trait Apply {
    /// Apply a given function in method-position.
    fn apply<F, U>(self, f: F) -> U
    where
        F: FnOnce(Self) -> U,
        Self: Sized;
}

impl<T> Apply for T {
    fn apply<F, U>(self, f: F) -> U
    where
        F: FnOnce(Self) -> U,
        Self: Sized,
    {
        f(self)
    }
}
