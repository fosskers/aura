//! Output a dependency graph in DOT format.

use aura_core::deps;
use r2d2_alpm::Alpm;

/// Given some packages to focus on, print their combined dependency graph in
/// DOT format.
pub(crate) fn graph(alpm: &Alpm, limit: Option<u8>, optional: bool, packages: Vec<String>) {
    let db = alpm.as_ref().localdb();
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let foreigns: Vec<_> = aura_core::foreign_packages(alpm)
        .map(|p| p.name())
        .collect();
    let graph = deps::PkgGraph::by_deps(db, limit, optional, &foreigns, &pkgs);

    println!("{}", graph);
}

/// Like [`graph`], but display all packages that depend on the given ones
/// instead.
pub(crate) fn reverse(alpm: &Alpm, limit: Option<u8>, optional: bool, packages: Vec<String>) {
    let db = alpm.as_ref().localdb();
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let foreigns: Vec<_> = aura_core::foreign_packages(alpm)
        .map(|p| p.name())
        .collect();
    let graph = deps::PkgGraph::by_parents(db, limit, optional, &foreigns, &pkgs);

    println!("{}", graph);
}
