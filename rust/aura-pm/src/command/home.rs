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
use crate::{aura, proceed};
use alpm::Alpm;
use alpm_utils::DbListExt;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use nonempty_collections::NEVec;
use std::ops::Not;
use std::path::{Path, PathBuf};
use validated::Validated;

pub(crate) enum Error {
    NoHome,
    /// The user specified packages that don't exist.
    NonExistant(NEVec<String>),
    NotALink(PathBuf),
    NoTarget(PathBuf),
    WrongTarget(PathBuf),
    Cancelled,
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
            Error::NotALink(p) => fl!(fll, "home-not-a-link", file = format!("{}", p.display())),
            Error::NoTarget(p) => fl!(fll, "home-no-target", file = format!("{}", p.display())),
            Error::WrongTarget(p) => {
                fl!(fll, "home-wrong-target", file = format!("{}", p.display()))
            }
            Error::Cancelled => fl!(fll, "common-cancelled"),
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
pub(crate) fn apply(fll: &FluentLanguageLoader, env: &Env, alpm: &Alpm) -> Result<(), Error> {
    let home = env.home.as_ref().ok_or(Error::NoHome)?;

    aura!(fll, "home-determine");

    let packages = pkgs_to_install(home, alpm)
        .ok()
        .map_err(Error::NonExistant)?;

    if packages.is_empty().not() {
        aura!(fll, "home-to-install");

        for p in packages.iter() {
            println!(" {p}");
        }
    }

    let config = env.xdg_config.as_path();

    let to_establish = home
        .links
        .iter()
        .filter_map(|sl| match sl.status(config) {
            LinkStatus::NotALink => Some(Err(Error::NotALink(sl.absolute(config)))),
            LinkStatus::NoTarget => Some(Err(Error::NoTarget(sl.absolute(config)))),
            LinkStatus::WrongTarget => Some(Err(Error::WrongTarget(sl.absolute(config)))),
            // A symlink is ready to be made.
            LinkStatus::NothingThere => Some(Ok(sl)),
            // We can safely ignore established, correct symlinks.
            LinkStatus::Established => None,
        })
        .collect::<Result<Vec<_>, _>>()?;

    if to_establish.is_empty().not() {
        aura!(fll, "home-symlinks");

        // To align the rendering of the symlinks.
        let longest = to_establish
            .iter()
            .map(|sl| sl.str_of_from().chars().count())
            .max()
            .unwrap_or(0);

        for sl in to_establish.iter() {
            println!(
                " {:w$} -> {}",
                sl.from().display(),
                sl.to().display(),
                w = longest
            );
        }
    }

    if packages.is_empty().not() || to_establish.is_empty().not() {
        proceed!(fll, "proceed").ok_or(Error::Cancelled)?;
    }

    Ok(())
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
