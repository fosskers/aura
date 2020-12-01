//! Dependency analysis internals.

use alpm::Db;
use petgraph::{graph::NodeIndex, Directed, Graph};
use std::{collections::HashMap, ops::Deref};

/// A dependency relationship between parent and child.
pub enum DepType {
    /// A true dependency required at runtime.
    Hard,
    /// An optional dependency for extra functionality.
    Opt,
    /// A dependency only necessary for compilation.
    Make,
    /// A dependency necessary for post-built tests.
    Check,
}

/// A unique collection of `Package`s and their dependencies.
pub struct PkgGraph<'a>(Graph<&'a str, DepType, Directed, u16>);

impl<'a> PkgGraph<'a> {
    /// Create a new `PkgGraph` with borrowed contents from an ALPM [`Db`].
    pub fn new(db: &'a Db, pkgs: &[&'a str]) -> Result<PkgGraph<'a>, alpm::Error> {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        for p in pkgs {
            PkgGraph::add_dep(db, &mut graph, &mut indices, p);
        }

        Ok(PkgGraph(graph))
    }

    /// Recursively add dependencies to the package `Graph`.
    fn add_dep(
        db: &'a Db,
        graph: &mut Graph<&'a str, DepType, Directed, u16>,
        indices: &mut HashMap<&'a str, NodeIndex<u16>>,
        parent: &'a str,
    ) -> Option<NodeIndex<u16>> {
        indices.get(parent).cloned().or_else(|| {
            db.pkg(parent).ok().map(|p| {
                let ix = graph.add_node(parent);
                indices.insert(parent, ix);

                for d in p.depends().iter() {
                    if let Some(dix) = PkgGraph::add_dep(db, graph, indices, d.name()) {
                        graph.add_edge(ix, dix, DepType::Hard);
                    }
                }

                ix
            })
        })
    }
}

impl std::fmt::Display for PkgGraph<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "digraph {{")?;
        let graph = &self.0;

        // Render nodes.
        for (ix, name) in graph.node_indices().map(|ix| (ix, graph[ix])) {
            writeln!(f, "    {} [ label=\"{}\"]", ix.index(), name)?;
        }

        writeln!(f, "")?;

        // Render edges.
        for e in graph.raw_edges().iter() {
            writeln!(f, "    {} -> {}", e.source().index(), e.target().index())?;
        }

        write!(f, "}}")
    }
}

// TODO Is this needed?
impl<'a> Deref for PkgGraph<'a> {
    type Target = Graph<&'a str, DepType, Directed, u16>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
