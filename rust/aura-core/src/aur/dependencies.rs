//! AUR package dependency solving.

use alpm::Alpm;
// use alpm_utils::DbListExt;
use chrono::Utc;
use disown::Disown;
// use itertools::Itertools;
use log::{debug, info};
use nonempty::NonEmpty;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use r2d2::{ManageConnection, Pool};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use srcinfo::Srcinfo;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use validated::Validated;

use crate::Apply;

/// Errors that can occur during dependency resolution.
#[derive(Debug)]
pub enum Error<E> {
    /// A [`Mutex`] was poisoned and couldn't be unlocked.
    PoisonedMutex,
    /// An error pulling from the resource pool.
    R2D2(r2d2::Error),
    /// An error parsing a `.SRCINFO` file.
    Srcinfo(srcinfo::Error),
    /// An error cloning or pulling a repo.
    Git(crate::git::Error),
    /// Multiple errors during concurrent dependency resolution.
    Resolutions(Box<NonEmpty<Error<E>>>),
    /// A named dependency does not exist.
    DoesntExist(String),
    /// A named dependency of some known package does not exist.
    DoesntExistWithParent(String, String),
    /// The dependency graph was somehow malformed. This should never occur.
    MalformedGraph,
    /// There was a cyclic dependency.
    CyclicDep(String),
    /// Contacting Faur somehow failed.
    Faur(E),
}

impl<E> From<crate::git::Error> for Error<E> {
    fn from(v: crate::git::Error) -> Self {
        Self::Git(v)
    }
}

impl<E> From<srcinfo::Error> for Error<E> {
    fn from(v: srcinfo::Error) -> Self {
        Self::Srcinfo(v)
    }
}

impl<E> From<r2d2::Error> for Error<E> {
    fn from(v: r2d2::Error) -> Self {
        Self::R2D2(v)
    }
}

impl<E: std::fmt::Display> std::fmt::Display for Error<E> {
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
            Error::DoesntExist(p) => write!(f, "{} is not a known package.", p),
            Error::DoesntExistWithParent(par, p) => {
                write!(f, "{}, required by {}, is not a known package.", p, par)
            }
            Error::Faur(e) => write!(f, "{}", e),
            Error::CyclicDep(e) => write!(f, "Cyclic dependency involving {}.", e),
            Error::MalformedGraph => write!(f, "The dependency graph was somehow malformed."),
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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl std::fmt::Display for Buildable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
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
pub fn resolve<'a, I, S, M, F, E>(
    pool: Pool<M>,
    fetch: &F,
    clone_dir: &Path,
    pkgs: I,
) -> Result<Resolution, Error<E>>
where
    I: IntoParallelIterator<Item = S>,
    S: AsRef<str> + Into<String>,
    M: ManageConnection<Connection = Alpm>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E> + Sync,
    E: Send,
{
    let arc = Arc::new(Mutex::new(Resolution::default()));

    let start = Utc::now();
    pkgs.into_par_iter()
        .map(|pkg| resolve_one(pool.clone(), arc.clone(), fetch, clone_dir, None, pkg))
        .collect::<Validated<(), Error<E>>>()
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

fn resolve_one<'a, S, M, F, E>(
    pool: Pool<M>,
    mutx: Arc<Mutex<Resolution>>,
    fetch: &F,
    clone_dir: &Path,
    parent: Option<&str>,
    pkg_raw: S,
) -> Result<(), Error<E>>
where
    S: AsRef<str> + Into<String>,
    M: ManageConnection<Connection = Alpm>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E> + Sync,
    E: Send,
{
    let pkg = strip_version(pkg_raw);
    let pr = pkg.as_str();

    // Drops the lock on the `Resolution` as soon as it can.
    let already_seen = {
        let res = mutx.lock().map_err(|_| Error::PoisonedMutex)?;
        res.seen(&pkg)
    };

    if !already_seen {
        debug!("{pr}");

        // Checks if the current package is installed or otherwise satisfied by
        // some package, and then immediately drops the ALPM handle.
        let (satisfied, start) = {
            let state = pool.state();
            debug!(
                "Trying to get ALPM handle ({} idle connections)",
                state.idle_connections
            );
            let alpm = pool.get()?;
            debug!("Got a handle.");
            let db = alpm.localdb();
            let start = Utc::now();
            let res = db.pkg(pr).is_ok() || db.pkgs().find_satisfier(pr).is_some();
            (res, start)
        };

        let end = Utc::now();
        let diff = end.timestamp_millis() - start.timestamp_millis();
        debug!("Satisfaction ({}) for {} in {}ms.", satisfied, pkg, diff);

        if satisfied {
            mutx.lock()
                .map_err(|_| Error::PoisonedMutex)?
                .satisfied
                .insert(pkg);
        } else {
            let alpm = pool.get()?;

            match alpm.syncdbs().find_satisfier(pr) {
                Some(official) => {
                    debug!("{} is an official package.", pr);

                    let prnt = official.name().to_string();

                    mutx.lock()
                        .map_err(|_| Error::PoisonedMutex)?
                        .to_install
                        .insert(Official(prnt.clone()))
                        .disown();

                    let deps: Vec<_> = official
                        .depends()
                        .into_iter()
                        .map(|d| d.name().to_string())
                        .collect();

                    // FIXME Fri Feb 18 15:07:23 2022
                    //
                    // Manual drops are a signal of bad design. For the moment
                    // these are necessary to avoid running out of ALPM handles
                    // when we recurse.
                    drop(official);
                    drop(alpm);

                    deps.into_par_iter()
                        .map(|d| {
                            resolve_one(
                                pool.clone(),
                                mutx.clone(),
                                fetch,
                                clone_dir,
                                Some(&prnt),
                                d,
                            )
                        })
                        .collect::<Validated<(), Error<E>>>()
                        .ok()
                        .map_err(|es| Error::Resolutions(Box::new(es)))?;
                }
                None => {
                    // FIXME Fri Feb 18 15:13:31 2022
                    //
                    // Same here as above.
                    drop(alpm);

                    debug!("{} is an AUR package.", pr);
                    let path = pull_or_clone(fetch, clone_dir, parent, &pkg)?;
                    debug!("Parsing .SRCINFO for {}", pkg);
                    let info = Srcinfo::parse_file(path.join(".SRCINFO"))?;
                    let name = info.base.pkgbase;

                    // --- Package identities provided by this one --- //
                    let mut prov = Vec::new();

                    // --- All possible deps to consider --- //
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
                            .for_each(|p| r.provided.insert(p).disown())
                    })?;

                    deps_copy
                        .into_par_iter()
                        .map(|p| {
                            let prnt = Some(parent.as_str());
                            resolve_one(pool.clone(), mutx.clone(), fetch, clone_dir, prnt, p)
                        })
                        .collect::<Validated<(), Error<E>>>()
                        .ok()
                        .map_err(|es| Error::Resolutions(Box::new(es)))?;
                }
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
fn pull_or_clone<S, F, E>(
    fetch: &F,
    clone_dir: &Path,
    parent: Option<S>,
    pkg: &str,
) -> Result<PathBuf, Error<E>>
where
    S: Into<String>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E>,
{
    // Best case scenario: We already have a local clone of the requested
    // dependency.
    if super::has_local_aur_clone(clone_dir, pkg) {
        let path = clone_dir.join(pkg);
        // crate::git::pull(&path)?; // Here. Potentially avoid this.
        Ok(path)
    } else {
        // Here, we don't have a local clone of the package, so we need to find
        // out if it's real.
        let mut info = crate::faur::info(std::iter::once(pkg), fetch).map_err(Error::Faur)?;
        let base = info
            // ASSUMPTION: The list is a singleton!
            .pop()
            // There were no immediate results, but the dependency might be
            // provided by something else.
            .or_else(|| {
                debug!("Trying extended provider search on {}.", pkg);
                crate::faur::provides(pkg, fetch)
                    .ok()
                    // FIXME Fri May 20 14:13:49 2022
                    //
                    // Somehow allow the user a choice of provider, if there are multiple.
                    // In general this should be unlikely on the AUR for the average user.
                    .and_then(|mut v| v.pop())
            })
            // Worst scenario: There wasn't a provider either. Then the
            // dependency, as requested, simply doesn't exist and we have to
            // halt the entire process.
            .ok_or_else(|| match parent {
                Some(par) => Error::DoesntExistWithParent(par.into(), pkg.to_string()),
                None => Error::DoesntExist(pkg.to_string()),
            })?
            .package_base;

        // Second best scenario: the requested dependency was part of some split
        // package (etc.) that we already know about.
        if super::has_local_aur_clone(clone_dir, &base) {
            let path = clone_dir.join(base);
            // crate::git::pull(&path)?; // Here. Potentially avoid this.
            Ok(path)
        } else {
            let path = crate::aur::clone_aur_repo(Some(clone_dir), &base)?;
            Ok(path)
        }
    }
}

/// Given a collection of [`Buildable`] packages, determine a tiered order in
/// which they should be built and installed together.
///
/// This ensures that all dependencies are built and installed before they're
/// needed.
pub fn build_order<E>(to_build: &[Buildable]) -> Result<Vec<Vec<&str>>, Error<E>> {
    info!("Determining build order.");

    let graph = dep_graph(to_build);

    petgraph::algo::toposort(&graph, None)
        .map_err(|cycle| match graph.node_weight(cycle.node_id()) {
            None => Error::MalformedGraph,
            Some(b) => Error::CyclicDep(b.to_string()),
        })?
        .into_iter()
        .map(|ix| graph.node_weight(ix))
        .collect::<Option<Vec<_>>>()
        .ok_or(Error::MalformedGraph)?
        .into_iter()
        // --- Split the packages by "layer" --- //
        .fold(
            (Vec::new(), Vec::new(), HashSet::new()),
            |(mut layers, mut group, mut deps), buildable| {
                if deps.contains(&buildable.name) {
                    layers.push(group);
                    deps.clear();
                    deps.extend(&buildable.deps);
                    (layers, vec![buildable.name.as_str()], deps)
                } else {
                    group.push(buildable.name.as_str());
                    deps.extend(&buildable.deps);
                    (layers, group, deps)
                }
            },
        )
        // --- Account for the final group --- //
        .apply(|(mut layers, group, _)| {
            if group.is_empty().not() {
                layers.push(group);
            }
            Ok(layers)
        })
}

/// Form a proper dependency graph.
fn dep_graph(to_build: &[Buildable]) -> Graph<&Buildable, ()> {
    let mut graph = Graph::new();
    let mut map: HashMap<&str, NodeIndex> = HashMap::new();

    for build in to_build {
        let ix = graph.add_node(build);
        map.insert(&build.name, ix);
    }

    for ix in map.values() {
        if let Some(build) = graph.node_weight(*ix) {
            for dep in build.deps.iter() {
                if let Some(oix) = map.get(dep.as_str()) {
                    graph.add_edge(*ix, *oix, ());
                }
            }
        }
    }

    graph
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

    #[test]
    fn trivial_graph() {
        let v = vec![
            Buildable {
                name: "a".to_string(),
                deps: vec!["b".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "b".to_string(),
                deps: HashSet::new(),
            },
        ];

        let g = dep_graph(&v);
        assert_eq!(v.len(), g.node_count());
        assert_eq!(1, g.edge_count());

        let o = build_order::<()>(&v).unwrap();
        assert_eq!(vec![vec!["a"], vec!["b"]], o);
    }
}
