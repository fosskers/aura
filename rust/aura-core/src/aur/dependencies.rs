//! AUR package dependency solving.

use alpm::Alpm;
use log::info;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use validated::Validated;

/// Errors that can occur during dependency resolution.
pub enum Error {
    /// A [`Mutex`] was poisoned and couldn't be unlocked.
    PoisonedMutex,
}

/// The results of dependency resolution.
#[derive(Default)]
pub struct Resolution<'a> {
    /// Packages to be installed from official repos.
    pub to_install: HashMap<&'a str, Official<'a>>,
    /// Packages to be built.
    pub to_build: HashMap<&'a str, Buildable<'a>>,
    /// Packages already installed on the system.
    pub satisfied: HashSet<&'a str>,
}

impl Resolution<'_> {
    /// Have we already considered the given package?
    pub fn seen(&self, pkg: &str) -> bool {
        self.to_install.contains_key(pkg)
            || self.to_build.contains_key(pkg)
            || self.satisfied.contains(pkg)
    }
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
    T: IntoParallelIterator<Item = S>,
    S: AsRef<str>,
{
    info!("Resolving dependencies.");

    let res = Arc::new(Mutex::new(Resolution::default()));

    pkgs.into_par_iter()
        .map(|p| resolve_one(res.clone(), p))
        .collect::<Validated<(), Error>>();

    todo!()
}

fn resolve_one<'a, S>(
    // alpm: &'a Alpm,
    mtx: Arc<Mutex<Resolution<'a>>>,
    pkg: S,
) -> Result<(), Error>
where
    S: AsRef<str>,
{
    let p = pkg.as_ref();
    let already_seen = {
        let res = mtx.lock().map_err(|_| Error::PoisonedMutex)?;
        res.seen(p)
    };

    if already_seen {
        Ok(())
    } else {
        todo!()
    }
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
