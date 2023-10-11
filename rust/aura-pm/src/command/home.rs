//! Specification of a consistent system environment that you can easily port to
//! other machines. It allows you to specify packages that you always want
//! installed, as well as automatically managed symlinks to various config
//! files.
//!
//! Obviously this is not as fully featured as Guix or Nix, but it does
//! formalize a basic setup that people often use Guix Home or write ad hoc
//! scripts for, and thus it rests somewhere between the two poles.

use crate::env::{Env, Home, LinkStatus};
use crate::error::Nested;
use crate::localization::Localised;
use alpm::Alpm;
use alpm_utils::DbListExt;
use i18n_embed_fl::fl;
use nonempty_collections::NEVec;
use std::path::Path;
use validated::Validated;

// Scenarios:
// - A new package has been added and isn't yet installed: install if exists.
// - A package was removed since the last invocation of `home`: do nothing.

pub(crate) enum Error {
    NoHome,
    /// The user specified packages that don't exist.
    NonExistant(NEVec<String>),
}

impl Nested for Error {
    fn nested(&self) {}
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::NoHome => fl!(fll, "home-nodef"),
            Error::NonExistant(ps) => {
                let pkgs = ps
                    .into_iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                fl!(fll, "home-non-existant", pkgs = pkgs)
            }
        }
    }
}

/// Are all the [`Symlink`]s defined in the `[home]` block well-defined?
pub(crate) fn links_established(home: &Home, xdg_config: &Path) -> bool {
    home.links
        .iter()
        .all(|sl| sl.status(xdg_config) == LinkStatus::Established)
}

/// Install all missing, expected packages, and establish any specified symlinks.
pub(crate) fn apply(env: &Env, alpm: &Alpm) -> Result<(), Error> {
    match env.home.as_ref() {
        None => Err(Error::NoHome),
        Some(home) => {
            let packages = pkgs_to_install(home, alpm)
                .ok()
                .map_err(Error::NonExistant)?;

            for p in packages {
                println!("{p}");
            }

            Ok(())
        }
    }
}

fn pkgs_to_install<'a>(home: &'a Home, alpm: &Alpm) -> Validated<Vec<&'a str>, String> {
    let dbs = alpm.syncdbs();
    let local = alpm.localdb();

    home.packages
        .iter()
        .filter_map(|p| {
            let s = p.as_str();

            if local.pkg(s).is_ok() {
                None // Already installed.
            } else if dbs.pkg(s).is_ok() {
                Some(Validated::Good(s)) // Not installed but could be.
            } else {
                Some(Validated::fail(s.to_string())) // Specified package doesn't exist.
            }
        })
        .collect::<Validated<Vec<_>, _>>()
}
