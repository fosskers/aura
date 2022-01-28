//! AUR package dependency solving.

use alpm::Alpm;
use log::info;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

/// Errors that can occur during dependency resolution.
pub enum Error {}

/// The results of dependency resolution.
pub struct Resolution<'a> {
    /// Packages to be installed from official repos.
    pub to_install: HashMap<&'a str, Official<'a>>,
    /// Packages to be built.
    pub to_build: HashMap<&'a str, Buildable<'a>>,
    /// Packages already installed on the system.
    pub satisfied: HashSet<&'a str>,
}

/// An official ALPM package.
pub struct Official<'a>(Cow<'a, str>);

/// A buildable package from the AUR.
pub struct Buildable<'a> {
    /// The name of the AUR package.
    pub name: Cow<'a, str>,
    /// The names of its dependencies.
    pub deps: Vec<Cow<'a, str>>,
}

/// Determine all packages to be built and installed.
pub fn resolve<'a, 'b, T, S>(alpm: &'a Alpm, pkgs: T) -> Result<Resolution<'a>, Error>
where
    T: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    info!("Resolving dependencies.");

    todo!()
}

/// Given a collection of [`Buildable`] packages, determine a tiered order in
/// which they should be built and installed together.
///
/// This ensures that all dependencies are built and installed before they're
/// needed.
pub fn build_order<'a, T>(to_build: T) -> Vec<Vec<Cow<'a, str>>>
where
    T: IntoIterator<Item = Buildable<'a>>,
{
    info!("Determining build order.");

    todo!()
}
