//! Output a dependency graph in DOT format.

use crate::error::Error;
use alpm::Alpm;
use aura_core::deps;

/// Given some packages to focus on, print their combined dependency graph in
/// DOT format.
pub fn graph(alpm: &Alpm, packages: Vec<String>) -> Result<(), Error> {
    let db = alpm.localdb();
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let graph = deps::PkgGraph::by_deps(&db, &pkgs)?;

    println!("{}", graph);
    Ok(())
}

/// Like [`graph`], but display all packages that depend on the given ones
/// instead.
pub fn reverse(alpm: &Alpm, packages: Vec<String>) -> Result<(), Error> {
    let db = alpm.localdb();
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let graph = deps::PkgGraph::by_parents(&db, &pkgs)?;

    println!("{}", graph);
    Ok(())
}
