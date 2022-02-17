//! Output a dependency graph in DOT format.

use alpm::Alpm;
use aura_core::deps;

/// Given some packages to focus on, print their combined dependency graph in
/// DOT format.
pub(crate) fn graph(
    alpm: &Alpm,
    limit: Option<u8>,
    optional: bool,
    packages: Vec<String>,
) -> Result<(), alpm::Error> {
    let db = alpm.localdb();
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let foreigns: Vec<_> = aura_arch::foreigns(alpm).map(|p| p.name()).collect();
    let graph = deps::PkgGraph::by_deps(&db, limit, optional, &foreigns, &pkgs)?;

    println!("{}", graph);
    Ok(())
}

/// Like [`graph`], but display all packages that depend on the given ones
/// instead.
pub(crate) fn reverse(
    alpm: &Alpm,
    limit: Option<u8>,
    optional: bool,
    packages: Vec<String>,
) -> Result<(), alpm::Error> {
    let db = alpm.localdb();
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let foreigns: Vec<_> = aura_arch::foreigns(alpm).map(|p| p.name()).collect();
    let graph = deps::PkgGraph::by_parents(&db, limit, optional, &foreigns, &pkgs)?;

    println!("{}", graph);
    Ok(())
}
