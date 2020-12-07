//! Common types across the library.

use std::path::Path;

/// The simplest form a package.
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
}
