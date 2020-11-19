use alpm::{Alpm, Package, PackageReason};

#[derive(Debug)]
pub enum Error {
    Alpm(alpm::Error),
}

/// Open an `Alpm` handle with default settings.
pub fn open_alpm() -> Result<Alpm, Error> {
    Alpm::new("/", "/var/lib/pacman/").map_err(Error::Alpm)
}

/// All orphaned packages.
///
/// An orphan is a package that was installed as a dependency, but whose parent
/// package is no longer installed.
pub fn orphans(alpm: &Alpm) -> Vec<Package> {
    alpm.localdb()
        .pkgs()
        .iter()
        .filter(|p| {
            p.reason() == PackageReason::Depend
                && p.required_by().is_empty()
                && p.optional_for().is_empty()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
