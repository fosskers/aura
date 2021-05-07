//! Common types across the library.

use std::cmp::Ordering;
use std::path::Path;

/// Like [`Package`], but its `String` contents are borrowed.
#[derive(Debug, PartialEq, Eq)]
pub struct Pkg<'a> {
    /// The name of the package.
    pub name: &'a str,
    /// The version of the package.
    pub version: &'a str,
}

impl<'a> Pkg<'a> {
    /// Construct a new `Pkg`.
    pub fn new(name: &'a str, version: &'a str) -> Pkg<'a> {
        Pkg { name, version }
    }
}

impl<'a> PartialOrd for Pkg<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Pkg<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.name.cmp(&other.name) {
            Ordering::Equal => alpm::vercmp(self.version, other.version),
            otherwise => otherwise,
        }
    }
}

/// The simplest form a package.
#[derive(Debug, PartialEq, Eq)]
pub struct Package {
    /// The name of the package.
    pub name: String,
    /// The version of the package.
    pub version: String,
}

impl Package {
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
    pub fn from_path(path: &Path) -> Option<Package> {
        path.file_name()
            .and_then(|file| file.to_str())
            .and_then(|file| file.rsplitn(2, '-').skip(1).next())
            .and_then(|pkg| {
                let mut vec: Vec<_> = pkg.rsplitn(3, '-').collect();
                let name = vec.last()?.to_string();
                vec.pop();
                vec.reverse();
                let version = vec.join("-");

                Some(Package { name, version })
            })
    }

    /// A borrowed variant of the internal `String` values.
    pub fn to_pkg(&self) -> Pkg {
        Pkg {
            name: self.name.as_str(),
            version: self.version.as_str(),
        }
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

impl PartialOrd for Package {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Package {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.name.cmp(&other.name) {
            Ordering::Equal => alpm::vercmp(self.version.as_str(), other.version.as_str()),
            otherwise => otherwise,
        }
    }
}
