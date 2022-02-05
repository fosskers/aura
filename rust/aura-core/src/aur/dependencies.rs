//! AUR package dependency solving.

use alpm::Alpm;
use alpm_utils::DbListExt;
use log::{debug, info};
use nonempty::NonEmpty;
use r2d2::{ManageConnection, Pool};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use srcinfo::Srcinfo;
use std::borrow::{Borrow, Cow};
use std::collections::HashSet;
use std::hash::Hash;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use validated::Validated;

/// Errors that can occur during dependency resolution.
pub enum Error {
    /// A [`Mutex`] was poisoned and couldn't be unlocked.
    PoisonedMutex,
    /// An error pulling from the resource pool.
    R2D2(r2d2::Error),
    /// An error parsing a `.SRCINFO` file.
    Srcinfo(srcinfo::Error),
    /// Multiple errors during concurrent dependency resolution.
    Resolutions(Box<NonEmpty<Error>>),
}

impl From<srcinfo::Error> for Error {
    fn from(v: srcinfo::Error) -> Self {
        Self::Srcinfo(v)
    }
}

impl From<r2d2::Error> for Error {
    fn from(v: r2d2::Error) -> Self {
        Self::R2D2(v)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::PoisonedMutex => write!(f, "Poisoned Mutex"),
            Error::R2D2(e) => write!(f, "{}", e),
            Error::Resolutions(es) => {
                writeln!(f, "Errors during dependency resolution:")?;
                for e in (*es).iter() {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
            Error::Srcinfo(e) => write!(f, "{}", e),
        }
    }
}

/// The results of dependency resolution.
#[derive(Default)]
pub struct Resolution<'a> {
    /// Packages to be installed from official repos.
    pub to_install: HashSet<Official<'a>>,
    /// Packages to be built.
    pub to_build: HashSet<Buildable<'a>>,
    /// Packages already installed on the system.
    pub satisfied: HashSet<Cow<'a, str>>,
    /// Packages that are somehow accounted for. A dependency might be provided
    /// by some package, but under a slightly different name. This also takes
    /// split packages into account.
    provided: HashSet<Cow<'a, str>>,
}

impl Resolution<'_> {
    /// Have we already considered the given package?
    pub fn seen(&self, pkg: &str) -> bool {
        self.provided.contains(pkg)
            || self.satisfied.contains(pkg)
            || self.to_install.contains(pkg)
            || self.to_build.contains(pkg)
    }
}

/// An official ALPM package.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Official<'a>(Cow<'a, str>);

impl Borrow<str> for Official<'_> {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl std::fmt::Display for Official<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A buildable package from the AUR.
#[derive(PartialEq, Eq)]
pub struct Buildable<'a> {
    /// The name of the AUR package.
    pub name: Cow<'a, str>,
    /// The names of its dependencies.
    pub deps: HashSet<Cow<'a, str>>,
}

impl Borrow<str> for Buildable<'_> {
    fn borrow(&self) -> &str {
        self.name.as_ref()
    }
}

impl Hash for Buildable<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

/// Determine all packages to be built and installed.
pub fn resolve<'a, I, S, M>(pool: Pool<M>, pkgs: I) -> Result<Resolution<'a>, Error>
where
    I: IntoParallelIterator<Item = S>,
    S: Into<Cow<'a, str>>,
    M: ManageConnection<Connection = Alpm>,
{
    let arc = Arc::new(Mutex::new(Resolution::default()));

    pkgs.into_par_iter()
        .map(|pkg| resolve_one(pool.clone(), arc.clone(), pkg))
        .collect::<Validated<(), Error>>()
        .ok()
        .map_err(|es| Error::Resolutions(Box::new(es)))?;

    let res = Arc::try_unwrap(arc)
        .map_err(|_| Error::PoisonedMutex)?
        .into_inner()
        .map_err(|_| Error::PoisonedMutex)?;

    Ok(res)
}

fn resolve_one<'a, S, M>(
    pool: Pool<M>,
    mutx: Arc<Mutex<Resolution<'a>>>,
    pkg: S,
) -> Result<(), Error>
where
    S: Into<Cow<'a, str>>,
    M: ManageConnection<Connection = Alpm>,
{
    let p = pkg.into();

    // Drops the lock on the `Resolution` as soon as it can.
    let already_seen = {
        let res = mutx.lock().map_err(|_| Error::PoisonedMutex)?;
        res.seen(&p)
    };

    if !already_seen {
        debug!("Resolving dependencies for: {}", p);

        // Checks if the current package is installed and drops the ALPM handle
        // as soon as possible.
        let installed = pool.get()?.localdb().pkg(p.as_ref()).is_ok();

        if installed {
            mutx.lock()
                .map_err(|_| Error::PoisonedMutex)?
                .satisfied
                .insert(p);
        } else {
            // Same here, re: lock dropping.
            let official = pool.get()?.syncdbs().pkg(p.as_ref()).is_ok();

            if official {
                mutx.lock()
                    .map_err(|_| Error::PoisonedMutex)?
                    .to_install
                    .insert(Official(p));
            } else {
                debug!("It's an AUR package!");
                let path = pull_or_clone(&p)?;
                let info = Srcinfo::parse_file(path)?;
                let name = Cow::Owned(info.base.pkgbase);
                let mut prov = Vec::new();
                let deps: HashSet<Cow<'_, str>> = info
                    .base
                    .makedepends
                    .into_iter()
                    .chain(info.pkg.depends)
                    .chain(
                        info.pkgs
                            .into_iter()
                            .map(|p| {
                                prov.push(p.pkgname);
                                p.depends
                            })
                            .flatten(),
                    )
                    .flat_map(|av| av.vec)
                    .map(Cow::Owned)
                    .collect();

                let buildable = Buildable { name, deps };

                mutx.lock().map_err(|_| Error::PoisonedMutex).map(|mut r| {
                    r.to_build.insert(buildable);

                    for p in info
                        .pkg
                        .provides
                        .into_iter()
                        .flat_map(|av| av.vec)
                        .chain(prov)
                    {
                        r.provided.insert(Cow::Owned(p));
                    }
                })?;

                // .map(|pkg| resolve_one(pool.clone(), mtx.clone(), pkg))
                // .collect::<Validated<(), Error>>()
                // .ok()
                // .map_err(|es| Error::Resolutions(Box::new(es)))?;
            }
        }
    }

    Ok(())
}

fn pull_or_clone(pkg: &str) -> Result<PathBuf, Error> {
    todo!()
}

/// Given a collection of [`Buildable`] packages, determine a tiered order in
/// which they should be built and installed together.
///
/// This ensures that all dependencies are built and installed before they're
/// needed.
pub fn build_order<'a, I>(to_build: I) -> Vec<Vec<Cow<'a, str>>>
where
    I: IntoIterator<Item = Buildable<'a>>,
{
    info!("Determining build order.");

    todo!()
}
