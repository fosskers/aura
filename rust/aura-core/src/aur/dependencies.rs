//! AUR package dependency solving.

use crate::utils::Void;
use alpm::Alpm;
use alpm_utils::DbListExt;
use chrono::Utc;
use log::{debug, info};
use nonempty::NonEmpty;
use r2d2::{ManageConnection, Pool};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use srcinfo::Srcinfo;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;
use std::path::{Path, PathBuf};
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
    /// An error cloning or pulling a repo.
    Git(crate::git::Error),
    /// An error contacting the AUR API.
    Raur(raur_curl::Error),
    /// Multiple errors during concurrent dependency resolution.
    Resolutions(Box<NonEmpty<Error>>),
    /// A named dependency does not exist.
    DoesntExist(String),
    /// A named dependency of some known package does not exist.
    DoesntExistWithParent(String, String),
}

impl From<raur_curl::Error> for Error {
    fn from(v: raur_curl::Error) -> Self {
        Self::Raur(v)
    }
}

impl From<crate::git::Error> for Error {
    fn from(v: crate::git::Error) -> Self {
        Self::Git(v)
    }
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
            Error::Git(e) => write!(f, "{}", e),
            Error::Raur(e) => write!(f, "{}", e),
            Error::DoesntExist(p) => write!(f, "{} is not a known package.", p),
            Error::DoesntExistWithParent(par, p) => {
                write!(f, "{}, required by {}, is not a known package.", p, par)
            }
        }
    }
}

/// The results of dependency resolution.
#[derive(Default)]
pub struct Resolution {
    /// Packages to be installed from official repos.
    pub to_install: HashSet<Official>,
    /// Packages to be built.
    pub to_build: HashSet<Buildable>,
    /// Packages already installed on the system.
    pub satisfied: HashSet<String>,
    /// Packages that are somehow accounted for. A dependency might be provided
    /// by some package, but under a slightly different name. This also takes
    /// split packages into account.
    provided: HashSet<String>,
}

impl Resolution {
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
pub struct Official(String);

impl Borrow<str> for Official {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl std::fmt::Display for Official {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A buildable package from the AUR.
#[derive(PartialEq, Eq)]
pub struct Buildable {
    /// The name of the AUR package.
    pub name: String,
    /// The names of its dependencies.
    pub deps: HashSet<String>,
}

impl std::fmt::Debug for Buildable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Buildable")
            .field("name", &self.name)
            .finish()
    }
}

impl Borrow<str> for Buildable {
    fn borrow(&self) -> &str {
        self.name.as_str()
    }
}

impl Hash for Buildable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

/// Determine all packages to be built and installed.
pub fn resolve<'a, I, S, M>(pool: Pool<M>, clone_dir: &Path, pkgs: I) -> Result<Resolution, Error>
where
    I: IntoParallelIterator<Item = S>,
    S: AsRef<str> + Into<String>,
    M: ManageConnection<Connection = Alpm>,
{
    let arc = Arc::new(Mutex::new(Resolution::default()));

    let start = Utc::now();
    pkgs.into_par_iter()
        .map(|pkg| resolve_one(pool.clone(), arc.clone(), clone_dir, None, pkg))
        .collect::<Validated<(), Error>>()
        .ok()
        .map_err(|es| Error::Resolutions(Box::new(es)))?;
    let end = Utc::now();
    let diff = end.timestamp_millis() - start.timestamp_millis();

    let res = Arc::try_unwrap(arc)
        .map_err(|_| Error::PoisonedMutex)?
        .into_inner()
        .map_err(|_| Error::PoisonedMutex)?;

    info!("Resolved dependencies in {}ms.", diff);

    Ok(res)
}

fn resolve_one<'a, S, M>(
    pool: Pool<M>,
    mutx: Arc<Mutex<Resolution>>,
    clone_dir: &Path,
    parent: Option<&str>,
    pkg_raw: S,
) -> Result<(), Error>
where
    S: AsRef<str> + Into<String>,
    M: ManageConnection<Connection = Alpm>,
{
    let pkg = strip_version(pkg_raw);
    let pr = pkg.as_str();

    // Drops the lock on the `Resolution` as soon as it can.
    let already_seen = {
        let res = mutx.lock().map_err(|_| Error::PoisonedMutex)?;
        res.seen(&pkg)
    };

    if !already_seen {
        // Checks if the current package is installed or otherwise satisfied by
        // some package, and then immediately drops the ALPM handle.
        let satisfied = {
            let alpm = pool.get()?;
            let db = alpm.localdb();
            let start = Utc::now();
            let res = db.pkg(pr).is_ok() || db.pkgs().find_satisfier(pr).is_some();
            let end = Utc::now();
            let diff = end.timestamp_millis() - start.timestamp_millis();
            debug!("AlpmList::find_satisfier for {} in {}ms", pkg, diff);
            res
        };

        if satisfied {
            mutx.lock()
                .map_err(|_| Error::PoisonedMutex)?
                .satisfied
                .insert(pkg);
        } else {
            // Same here, re: lock dropping.
            // TODO Wed Feb  9 22:41:15 2022
            //
            // Also need to do `find_satisfier` here!
            let official = pool.get()?.syncdbs().pkg(pr).is_ok();

            if official {
                // TODO Wed Feb  9 22:10:23 2022
                //
                // Recurse on the package for its dependencies.
                mutx.lock()
                    .map_err(|_| Error::PoisonedMutex)?
                    .to_install
                    .insert(Official(pkg));
            } else {
                let path = pull_or_clone(clone_dir, parent, &pkg)?;
                debug!("Parsing .SRCINFO for {}", pkg);
                let info = Srcinfo::parse_file(path.join(".SRCINFO"))?;
                let name = info.base.pkgbase;
                let mut prov = Vec::new();
                let deps: HashSet<String> = info
                    .base
                    .makedepends
                    .into_iter()
                    .chain(info.pkg.depends)
                    .chain(
                        info.pkgs
                            .into_iter()
                            .map(|p| {
                                // Sneak out this package's name as a "provided name".
                                prov.push(p.pkgname);
                                p.depends
                            })
                            .flatten(),
                    )
                    .flat_map(|av| av.vec)
                    .collect();

                let deps_copy: Vec<String> = deps.iter().map(|d| d.clone()).collect();
                let parent = name.clone();
                let buildable = Buildable { name, deps };

                mutx.lock().map_err(|_| Error::PoisonedMutex).map(|mut r| {
                    r.to_build.insert(buildable);

                    info.pkg
                        .provides
                        .into_iter()
                        .flat_map(|av| av.vec)
                        .chain(prov)
                        .for_each(|p| r.provided.insert(p).void())
                })?;

                deps_copy
                    .into_iter()
                    .map(|p| {
                        let prnt = Some(parent.as_str());
                        resolve_one(pool.clone(), mutx.clone(), clone_dir, prnt, p)
                    })
                    .collect::<Validated<(), Error>>()
                    .ok()
                    .map_err(|es| Error::Resolutions(Box::new(es)))?;
            }
        }
    }

    Ok(())
}

// FIXME Mon Feb  7 23:07:56 2022
//
// If `is_aur_package_fast` succeeds, perhaps we should assume that the clone is
// up to date and avoid a pull here to speed things up. It may be better to
// encourage usage of `-Ay`.
//
// Of course if there is no local clone, then a fresh one must be done either
// way, ensuring newness for at least that run.
//
// The goal here is to rely on our local clone more, to avoid having to call to
// the AUR all the time. `-Ai`, perhaps, should also read local clones if they
// exist. This offers the bonus of `-Ai` functioning offline, like `-Si` does!
fn pull_or_clone<S>(clone_dir: &Path, parent: Option<S>, pkg: &str) -> Result<PathBuf, Error>
where
    S: Into<String>,
{
    if super::is_aur_package_fast(clone_dir, pkg) {
        let path = clone_dir.join(pkg);
        // crate::git::pull(&path)?; // Here. Potentially avoid this.
        Ok(path)
    } else {
        let info = crate::aur::info(&[pkg])?;
        let base = info
            .first()
            .ok_or_else(|| match parent {
                Some(par) => Error::DoesntExistWithParent(par.into(), pkg.to_string()),
                None => Error::DoesntExist(pkg.to_string()),
            })?
            .package_base
            .as_str();

        // FIXME Wed Feb  9 21:24:27 2022
        //
        // Avoid the code duplication.
        if super::is_aur_package_fast(clone_dir, base) {
            let path = clone_dir.join(base);
            // crate::git::pull(&path)?; // Here. Potentially avoid this.
            Ok(path)
        } else {
            let path = crate::aur::clone_aur_repo(Some(clone_dir), base)?;
            Ok(path)
        }
    }
}

/// Given a collection of [`Buildable`] packages, determine a tiered order in
/// which they should be built and installed together.
///
/// This ensures that all dependencies are built and installed before they're
/// needed.
pub fn build_order<I>(to_build: I) -> Vec<Vec<String>>
where
    I: IntoIterator<Item = Buildable>,
{
    info!("Determining build order.");

    todo!()
}

/// Strip version demands from a dependency string.
fn strip_version<'a, S>(stri: S) -> String
where
    S: AsRef<str> + Into<String>,
{
    stri.as_ref()
        .split_once(['=', '>'])
        .map(|(good, _)| good.to_string())
        .unwrap_or_else(|| stri.into())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn version_stripping() {
        assert_eq!("gcc6", strip_version("gcc6=6.5.0-7"));
        assert_eq!("glibc", strip_version("glibc>=2.25"));
    }
}
