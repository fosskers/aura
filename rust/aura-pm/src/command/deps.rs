//! Output a dependency graph in DOT format.

use crate::error::Nested;
use crate::localization::Localised;
use applying::Apply;
use aura_core::deps;
use aura_core::deps::PkgGraph;
use aura_core::Dbs;
use i18n_embed_fl::fl;
use log::error;
use r2d2_alpm::Alpm;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;

pub(crate) enum Error {
    Io(std::io::Error),
    Stdin,
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Io(e) => error!("{e}"),
            Error::Stdin => {}
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::Io(_) => fl!(fll, "deps-io"),
            Error::Stdin => fl!(fll, "deps-io"),
        }
    }
}

/// Given some packages to focus on, output their combined dependency graph as a
/// PNG, unless requested to be printed in raw DOT format.
pub(crate) fn graph(
    alpm: &Alpm,
    limit: Option<u8>,
    optional: bool,
    raw: bool,
    open: bool,
    packages: Vec<String>,
) -> Result<(), Error> {
    let db = Dbs::from_alpm(alpm);
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let foreigns: Vec<_> = aura_core::foreign_packages(alpm)
        .map(|p| p.name())
        .collect();
    let graph = deps::PkgGraph::by_deps(&db, limit, optional, &foreigns, &pkgs);

    if raw {
        println!("{}", graph);
    } else {
        render(graph, &packages, open)?;
    }

    Ok(())
}

/// Like [`graph`], but display all packages that depend on the given ones
/// instead.
pub(crate) fn reverse(
    alpm: &Alpm,
    limit: Option<u8>,
    optional: bool,
    raw: bool,
    open: bool,
    packages: Vec<String>,
) -> Result<(), Error> {
    let db = Dbs::from_alpm(alpm);
    let pkgs: Vec<_> = packages.iter().map(|p| p.as_ref()).collect();
    let foreigns: Vec<_> = aura_core::foreign_packages(alpm)
        .map(|p| p.name())
        .collect();
    let graph = deps::PkgGraph::by_parents(&db, limit, optional, &foreigns, &pkgs);

    if raw {
        println!("{}", graph);
    } else {
        render(graph, &packages, open)?;
    }

    Ok(())
}

fn render(graph: PkgGraph, pkgs: &[String], open: bool) -> Result<(), Error> {
    let name: PathBuf = if pkgs.is_empty() {
        PathBuf::from("deps.png")
    } else {
        itertools::intersperse(pkgs.iter().map(|s| s.as_str()), "-")
            .collect::<String>()
            .apply(PathBuf::from)
            .apply(|p| p.with_extension("png"))
    };

    let path = if open { Path::new("/tmp").join(name) } else { name };

    let mut child = Command::new("dot")
        .arg("-Tpng")
        .arg("-o")
        .arg(&path)
        .stdin(Stdio::piped())
        .spawn()
        .map_err(Error::Io)?;

    let g_string = graph.to_string();
    let mut stdin = child.stdin.take().ok_or(Error::Stdin)?;
    std::thread::spawn(move || stdin.write_all(g_string.as_bytes()));
    child.wait().map_err(Error::Io)?;

    if open {
        Command::new("xdg-open")
            .arg(path)
            .status()
            .map_err(Error::Io)?;
    }

    Ok(())
}
