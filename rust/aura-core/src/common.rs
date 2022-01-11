//! Common types across the library.

use std::borrow::Cow;
use std::cmp::Ordering;
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
    /// use aura_core::common::Package;
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
            .and_then(|file| file.rsplitn(2, '-').nth(1))
            .and_then(|pkg| {
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
