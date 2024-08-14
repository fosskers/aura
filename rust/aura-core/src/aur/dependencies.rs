//! AUR package dependency solving.

use applying::Apply;
use log::debug;
use log::info;
use nonempty_collections::nev;
use nonempty_collections::NESet;
use nonempty_collections::NEVec;
use nonempty_collections::NonEmptyIterator;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use r2d2::ManageConnection;
use r2d2::Pool;
use r2d2_alpm::Alpm;
use rayon::iter::IntoParallelIterator;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;
use srcinfo::Srcinfo;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Not;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use time::OffsetDateTime;
use validated::Validated;

/// Errors that can occur during dependency resolution.
#[derive(Debug)]
pub enum Error<E> {
    /// A [`Mutex`] was poisoned and couldn't be unlocked.
    PoisonedMutex,
    /// An error pulling from the resource pool.
    R2D2(r2d2::Error),
    /// An error parsing a `.SRCINFO` file.
    Srcinfo(PathBuf, srcinfo::Error),
    /// An error cloning or pulling a repo.
    Git(crate::git::Error),
    /// Multiple errors during concurrent dependency resolution.
    Resolutions(Box<NEVec<Error<E>>>),
    /// A named dependency does not exist.
    DoesntExist(String),
    /// A named dependency of some known package does not exist.
    DoesntExistWithParent(String, String),
    /// The dependency graph was somehow malformed. This should never occur.
    MalformedGraph,
    /// There was a cyclic dependency.
    CyclicDep(Vec<String>),
    /// Contacting Faur somehow failed.
    Faur(E),
}

impl<E> Error<E> {
    /// A flattened list of all inner error values.
    pub fn inner_errors(&self) -> NEVec<&Self> {
        match self {
            Error::Resolutions(bx) => bx.as_ref().iter().flat_map(|e| e.inner_errors()).collect(),
            otherwise => nev![otherwise],
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

    /// Set the given packages as the ones to build without any other
    /// considerations.
    pub fn build_these<I, S>(pkgs: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let to_build = pkgs
            .into_iter()
            .map(|s| {
                let name = s.as_ref().into();
                let deps = HashSet::new();
                Buildable { name, deps }
            })
            .collect();

        Resolution {
            to_install: HashSet::new(),
            to_build,
            satisfied: HashSet::new(),
            provided: HashSet::new(),
        }
    }
}

/// An official ALPM package.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Official(String);

impl Official {
    /// Construct an `Official`.
    pub fn new<S>(s: S) -> Self
    where
        S: Into<String>,
    {
        Official(s.into())
    }
}

impl Borrow<str> for Official {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl AsRef<str> for Official {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl std::fmt::Display for Official {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A buildable package from the AUR.
#[derive(Eq)]
pub struct Buildable {
    /// The name of the AUR package.
    pub name: String,
    /// The names of its dependencies, both official and AUR.
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

impl PartialEq for Buildable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Buildable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

fn confirm_base_devel<M, E>(pool: Pool<M>, mutx: Arc<Mutex<Resolution>>) -> Result<(), Error<E>>
where
    M: ManageConnection<Connection = Alpm>,
{
    let alpm = pool.get().map_err(Error::R2D2)?;
    let db = alpm.alpm.localdb();

    if db.pkg("base-devel").is_err() {
        let p = Official::new("base-devel");

        mutx.lock()
            .map_err(|_| Error::PoisonedMutex)?
            .to_install
            .insert(p);
    }

    Ok(())
}

/// Determine all packages to be built and installed.
pub fn resolve<M, F, E>(
    pool: Pool<M>,
    fetch: &F,
    clone_d: &Path,
    nocheck: bool,
    pkgs: &HashSet<&str>,
) -> Result<Resolution, Error<E>>
where
    M: ManageConnection<Connection = Alpm>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E> + Sync,
    E: Send,
{
    let arc = Arc::new(Mutex::new(Resolution::default()));

    // The Arch Wiki states that `base-devel` is to be considered an implicit
    // (make-)dependency of every other package. Here we add it automatically if
    // the user doesn't have it installed.
    confirm_base_devel(pool.clone(), arc.clone())?;

    let start = OffsetDateTime::now_utc();
    pkgs.par_iter()
        .map(|pkg| {
            let pool = pool.clone();
            resolve_one(pool, arc.clone(), fetch, clone_d, pkgs, None, pkg, nocheck)
        })
        .collect::<Validated<(), Error<E>>>()
        .ok()
        .map_err(|es| Error::Resolutions(Box::new(es)))?;
    let end = OffsetDateTime::now_utc();
    let diff = end.unix_timestamp() - start.unix_timestamp();

    let res = Arc::try_unwrap(arc)
        .map_err(|_| Error::PoisonedMutex)?
        .into_inner()
        .map_err(|_| Error::PoisonedMutex)?;

    info!("Resolved dependencies in {}s.", diff);

    Ok(res)
}

#[allow(clippy::too_many_arguments)]
fn resolve_one<M, F, E>(
    pool: Pool<M>,
    mutx: Arc<Mutex<Resolution>>,
    fetch: &F,
    clone_d: &Path,
    orig: &HashSet<&str>,
    parent: Option<&str>,
    pkg_raw: &str,
    nocheck: bool,
) -> Result<(), Error<E>>
where
    M: ManageConnection<Connection = Alpm>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E> + Sync,
    E: Send,
{
    let pkg: String = strip_version(pkg_raw);
    let pr = pkg.as_str();

    // Drops the lock on the `Resolution` as soon as it can.
    let already_seen = {
        let res = mutx.lock().map_err(|_| Error::PoisonedMutex)?;
        res.seen(&pkg)
    };

    if !already_seen {
        // debug!("{pr}");

        // Checks if the current package is installed or otherwise satisfied by
        // some package, and then immediately drops the ALPM handle.
        let satisfied = {
            // let state = pool.state();
            // debug!(
            //     "Trying to get ALPM handle ({} idle connections)",
            //     state.idle_connections
            // );
            let alpm = pool.get().map_err(Error::R2D2)?;
            // debug!("Got a handle.");
            let db = alpm.alpm.localdb();
            db.pkg(pr).is_ok() || db.pkgs().find_satisfier(pr).is_some()
        };

        debug!("Satisfaction ({}) for {}.", satisfied, pkg);

        if orig.contains(pr).not() && satisfied {
            mutx.lock()
                .map_err(|_| Error::PoisonedMutex)?
                .satisfied
                .insert(pkg);
        } else {
            let alpm = pool.get().map_err(Error::R2D2)?;

            match alpm.alpm.syncdbs().find_satisfier(pr) {
                Some(official) => {
                    debug!("{} is an official package.", pr);

                    let prnt = official.name().to_string();

                    mutx.lock()
                        .map_err(|_| Error::PoisonedMutex)?
                        .to_install
                        .insert(Official::new(&prnt));

                    // Since this is an official, prebuilt package, we don't
                    // need to consider its makedeps or checkdeps.
                    let deps: Vec<_> = official
                        .depends()
                        .into_iter()
                        .map(|d| d.name().to_string())
                        .collect();

                    // FIXME Fri Feb 18 2022 Avoid manual drops.
                    //
                    // Manual drops are a signal of bad design. For the moment
                    // these are necessary to avoid running out of ALPM handles
                    // when we recurse.
                    drop(alpm);

                    deps.into_par_iter()
                        .map(|d| {
                            let p = Some(prnt.as_str());
                            let pool = pool.clone();
                            resolve_one(pool, mutx.clone(), fetch, clone_d, orig, p, &d, nocheck)
                        })
                        .collect::<Validated<(), Error<E>>>()
                        .ok()
                        .map_err(|es| Error::Resolutions(Box::new(es)))?;
                }
                None => {
                    // FIXME Fri Feb 18 2022 Same here as above.
                    drop(alpm);

                    debug!("{} is may be an AUR package.", pr);
                    let path = pull_or_clone(fetch, clone_d, parent, &pkg)?;
                    debug!("Parsing .SRCINFO for {}", pkg);
                    let full = path.join(".SRCINFO");
                    let info = Srcinfo::parse_file(&full).map_err(|e| Error::Srcinfo(full, e))?;
                    let name = info.base.pkgbase;

                    // --- Package identities provided by this one --- //
                    let prov: HashSet<_> = info.pkgs.iter().map(|p| p.pkgname.clone()).collect();

                    // --- All possible deps to consider --- //
                    let deps: HashSet<String> = info
                        .base
                        .makedepends
                        .into_iter()
                        .chain(info.pkg.depends)
                        .chain(info.pkgs.into_iter().flat_map(|p| p.depends))
                        .chain(respect_checkdeps(nocheck, info.base.checkdepends))
                        .flat_map(|av| av.vec)
                        .map(strip_version)
                        // To prevent false detection of dependency cycles
                        // during build preparation.
                        //
                        // Consider the case of `mingw-w64-harfbuzz-icu`, a
                        // split package found within `mingw-w64-harfbuzz`. The
                        // former depends on the latter (at runtime), but they
                        // are built as a pair during the same `makepkg`
                        // invocation. So we don't have to consider normal
                        // dependency relationships between them.
                        .filter(|p| prov.contains(p).not())
                        .collect();

                    debug!("{} ({}) => {:?}", pr, name, deps);

                    let deps_copy: Vec<String> = deps.iter().cloned().collect();
                    let parent = name.clone();
                    let buildable = Buildable { name, deps };

                    mutx.lock().map_err(|_| Error::PoisonedMutex).map(|mut r| {
                        r.to_build.insert(buildable);

                        info.pkg
                            .provides
                            .into_iter()
                            .flat_map(|av| av.vec)
                            .chain(prov)
                            .map(strip_version)
                            .for_each(|p| {
                                r.provided.insert(p);
                            })
                    })?;

                    deps_copy
                        .into_par_iter()
                        .map(|p| {
                            let prnt = Some(parent.as_str());
                            let pool = pool.clone();
                            resolve_one(pool, mutx.clone(), fetch, clone_d, orig, prnt, &p, nocheck)
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

/// Consider "checkdeps" as well, unless specifically instructed not to.
fn respect_checkdeps<T>(nocheck: bool, deps: Vec<T>) -> Vec<T> {
    if nocheck {
        Vec::new()
    } else {
        deps
    }
}

// FIXME Mon Feb 7 2022 pull_or_clone
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
    clone_d: &Path,
    parent: Option<S>,
    pkg: &str,
) -> Result<PathBuf, Error<E>>
where
    S: Into<String>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E>,
{
    // Best case scenario: We already have a local clone of the requested
    // dependency.
    if super::has_local_aur_clone(clone_d, pkg) {
        let path = clone_d.join(pkg);
        // crate::git::pull(&path)?; // Here. Potentially avoid this.
        Ok(path)
    } else {
        // Here, we don't have a local clone of the package, so we need to find
        // out if it's real.
        let mut info = crate::faur::info([pkg], fetch).map_err(Error::Faur)?;
        let base = info
            // ASSUMPTION: The list is a singleton!
            .pop()
            // There were no immediate results, but the dependency might be
            // provided by something else.
            .or_else(|| {
                debug!("Trying extended provider search on {}.", pkg);
                crate::faur::provides(pkg, fetch)
                    .ok()
                    // FIXME Fri May 20 2022 Somehow allow the user a choice of provider, if there are multiple.
                    // In general this should be unlikely on the AUR for the average user, especially for dependencies.
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
        if super::has_local_aur_clone(clone_d, &base) {
            let path = clone_d.join(base);
            // crate::git::pull(&path)?; // Here. Potentially avoid this.
            Ok(path)
        } else {
            let path = crate::aur::clone_aur_repo(Some(clone_d), &base).map_err(Error::Git)?;
            Ok(path)
        }
    }
}

/// Given a collection of [`Buildable`] packages, determine a tiered order in
/// which they should be built and installed together.
///
/// This ensures that all dependencies are built and installed before they're
/// needed.
///
/// ```
/// let res = aura_core::aur::dependencies::build_order::<()>(vec![]).unwrap();
/// assert!(res.is_empty());
/// ```
pub fn build_order<E>(mut to_build: Vec<Buildable>) -> Result<Vec<Vec<String>>, Error<E>> {
    info!("Determining build order.");

    // NOTE 2024-08-15 This re-sorting is a workaround around an issue with the
    // toposort results that occurs due to a combination of:
    //
    //   1. Packages having AUR dependencies.
    //   2. Package names having a certain alphabetical ordering.
    //   3. The insertion order in the input Vec relative to that alphabetical ordering.
    //
    // Experimentally, it seems that by sorting by dep-count we can avoid the
    // issue. Note also that this is almost certainly the same issue I was
    // seeing previously of "Aura uninstalling itself" due to the effects of
    // `-a`.
    to_build.sort_by_key(|b| b.deps.len());
    to_build.reverse();
    let graph = dep_graph(&to_build);

    // NOTE Testing only.
    // let dot = Dot::new(&graph);
    // std::fs::write("dep-graph.dot", format!("{:?}", dot)).unwrap();

    let topo = petgraph::algo::toposort(&graph, None)
        .map_err(|cycle| {
            shortest_cycle(cycle.node_id(), &graph)
                .into_iter()
                .filter_map(|ix| graph.node_weight(ix))
                .map(|b| b.to_string())
                .collect::<Vec<_>>()
                .apply(Error::CyclicDep)
        })?
        .into_iter()
        // Least-depended-upon to most-dependend-upon ordering. We reverse the
        // order again at the very end.
        .map(|ix| graph.node_weight(ix))
        .collect::<Option<Vec<_>>>()
        .ok_or(Error::MalformedGraph)?;

    topo.into_iter()
        // --- Split the packages by "layer" --- //
        .fold(
            (Vec::new(), Vec::new(), HashSet::new()),
            |(mut layers, mut group, mut deps), buildable| {
                if deps.contains(&buildable.name) {
                    // Then, this buildable can't be built in the same layer as
                    // the previously considered packages. We thus construct a
                    // new layer.
                    layers.push(group);
                    deps.clear();
                    // NOTE 2024-08-07 Potentially a subtle bug here.
                    //
                    // We blindly add all this `buildable`'s deps here and hope
                    // that thanks to the topological sort that they've already
                    // processed into early layers. Also see comment (1) below.
                    deps.extend(&buildable.deps);
                    (layers, vec![buildable.name.to_string()], deps)
                } else {
                    // (1) While all of the Buildable's official (non-AUR) deps
                    // are also included in its `deps` field, only its own name
                    // is ever added to the build order "group", and thus we
                    // never try to mistakenly build an official package as an
                    // AUR one.
                    group.push(buildable.name.to_string());
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
            // The most-dependend-upon packages will now come first.
            layers.reverse();
            Ok(layers)
        })
}

fn shortest_cycle<N, E>(ix: NodeIndex, graph: &Graph<N, E>) -> Vec<NodeIndex> {
    petgraph::algo::all_simple_paths::<Vec<_>, _>(&graph, ix, ix, 0, None)
        .fold(None, |acc, cycle| match acc {
            None => Some(cycle),
            Some(v) if cycle.len() < v.len() => Some(cycle),
            _ => acc,
        })
        .unwrap_or_default()
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

/// Strip version demands from a dependency string, if any.
fn strip_version<S>(stri: S) -> String
where
    S: AsRef<str> + Into<String>,
{
    stri.as_ref()
        .split_once(['=', '>'])
        .map(|(good, _)| good.to_string())
        .unwrap_or_else(|| stri.into())
}

/// Interdependency relationships just within a given package.
pub struct Interdeps<'a>(HashMap<&'a str, Vec<&'a str>>);

impl<'a> Interdeps<'a> {
    /// Construct a mapping of interdependency relationships given a specific [`Srcinfo`].
    pub fn from_srcinfo(srcinfo: &'a Srcinfo) -> Self {
        let reals: HashSet<_> = srcinfo.pkgs.iter().map(|p| p.pkgname.as_str()).collect();

        srcinfo
            .pkgs
            .iter()
            .map(|p| {
                let ds = p
                    .depends
                    .iter()
                    .flat_map(|av| av.vec.as_slice())
                    .filter(|d| reals.contains(d.as_str()))
                    .map(|s| s.as_str())
                    .collect();
                (p.pkgname.as_str(), ds)
            })
            .collect::<HashMap<_, _>>()
            .apply(Interdeps)
    }

    /// Starting from some given package name, all the packages that belong to
    /// its transitive chain of dependencies.
    pub fn transitive(&'a self, head: &'a str) -> Option<NESet<&'a str>> {
        let mut chain = self.transitive_work(head, HashSet::new());

        if chain.is_empty() {
            None
        } else {
            chain.insert(head);
            NESet::from_set(chain)
        }
    }

    fn transitive_work(&'a self, dep: &'a str, curr: HashSet<&'a str>) -> HashSet<&'a str> {
        match self.0.get(dep) {
            None => curr,
            Some(deps) => deps.iter().fold(curr, |mut acc, d| {
                // Prevents infinite recursion, even though we don't expect
                // there to be dependency cycles in any place where this would
                // be used.
                if acc.contains(d) {
                    acc
                } else {
                    acc.insert(d);
                    self.transitive_work(d, acc)
                }
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use nonempty_collections::nes;

    #[test]
    fn interdeps() {
        let qlot = Srcinfo::parse_file("tests/qlot.SRCINFO").unwrap();
        let deps = Interdeps::from_srcinfo(&qlot);
        let mut expt = HashMap::new();
        expt.insert("qlot", Vec::new());
        assert_eq!(expt, deps.0);

        let nx = Srcinfo::parse_file("tests/nx.SRCINFO").unwrap();
        let deps = Interdeps::from_srcinfo(&nx);
        let mut expt = HashMap::new();
        expt.insert("libxcomp", Vec::new());
        expt.insert("nxproxy", vec!["libxcomp"]);
        expt.insert("nx-x11", vec!["libxcomp"]);
        expt.insert("nxagent", vec!["nx-x11", "libxcomp"]);
        expt.insert("nx-headers", Vec::new());
        assert_eq!(expt, deps.0);

        let trans = deps.transitive("nxproxy").unwrap();
        assert_eq!(nes!["nxproxy", "libxcomp"], trans);
    }

    #[test]
    fn version_stripping() {
        assert_eq!("gcc6", strip_version("gcc6"));
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

        let o = build_order::<()>(v).unwrap();
        assert_eq!(vec![vec!["b"], vec!["a"],], o);
    }

    #[test]
    fn diamond_graph() {
        let v = vec![
            Buildable {
                name: "a".to_string(),
                deps: vec!["b".to_string(), "c".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "b".to_string(),
                deps: vec!["d".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "c".to_string(),
                deps: vec!["d".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "d".to_string(),
                deps: HashSet::new(),
            },
        ];

        let g = dep_graph(&v);
        assert_eq!(v.len(), g.node_count());
        assert_eq!(4, g.edge_count());

        let mut o = build_order::<()>(v).unwrap();
        for v in o.iter_mut() {
            v.sort();
        }
        assert_eq!(vec![vec!["d"], vec!["b", "c"], vec!["a"]], o);
    }

    #[test]
    fn medium_graph() {
        let v = vec![
            Buildable {
                name: "a".to_string(),
                deps: vec!["b".to_string(), "c".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "b".to_string(),
                deps: vec!["d".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "c".to_string(),
                deps: vec!["d".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "e".to_string(),
                deps: vec!["d".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "d".to_string(),
                deps: HashSet::new(),
            },
            Buildable {
                name: "f".to_string(),
                // deps: vec!["c".to_string()].into_iter().collect(),
                deps: HashSet::new(),
            },
        ];

        let g = dep_graph(&v);
        assert_eq!(v.len(), g.node_count());
        assert_eq!(5, g.edge_count());

        let mut o = build_order::<()>(v).unwrap();
        for v in o.iter_mut() {
            v.sort();
        }
        assert_eq!(vec![vec!["d"], vec!["b", "c"], vec!["a", "e", "f"]], o);
    }

    #[test]
    fn real_failing_graph() {
        let v = vec![
            Buildable {
                name: "mgba-git".to_string(),
                deps: HashSet::new(),
            },
            Buildable {
                name: "sway-git".to_string(),
                deps: vec!["wlroots-git".to_string()].into_iter().collect(),
            },
            Buildable {
                name: "wlroots-git".to_string(),
                deps: HashSet::new(),
            },
            Buildable {
                name: "timelineproject-hg".to_string(),
                deps: HashSet::new(),
            },
        ];

        let g = dep_graph(&v);
        assert_eq!(v.len(), g.node_count());
        assert_eq!(1, g.edge_count());

        let mut o = build_order::<()>(v).unwrap();
        for v in o.iter_mut() {
            v.sort();
        }
        assert_eq!(
            vec![
                vec!["wlroots-git"],
                vec!["mgba-git", "sway-git", "timelineproject-hg"]
            ],
            o
        );
    }
}
