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
    // /// A dependency only necessary for compilation.
    // Make,
    // /// A dependency necessary for post-built tests.
    // Check,
}

impl DepType {
    /// The line styling for an edge.
    fn edge_style(&self) -> &'static str {
        match self {
            DepType::Hard => "style=solid",
            DepType::Opt => "style=dotted",
        }
    }
}

/// A unique collection of `Package`s and their dependencies.
pub struct PkgGraph<'a>(Graph<&'a str, DepType, Directed, u16>);

impl<'a> PkgGraph<'a> {
    /// Create a new `PkgGraph` of given packages and all their dependencies.
    pub fn by_deps(db: &'a Db, pkgs: &[&'a str]) -> Result<PkgGraph<'a>, alpm::Error> {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        for p in pkgs {
            PkgGraph::add_dep(db, &mut graph, &mut indices, p);
        }

        Ok(PkgGraph(graph))
    }

    /// Create a new `PkgGraph` of given packages and all packages that require them.
    pub fn by_parents(db: &'a Db, pkgs: &[&'a str]) -> Result<PkgGraph<'a>, alpm::Error> {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        for p in pkgs {
            PkgGraph::add_parent(db, &mut graph, &mut indices, p);
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

                // Dependencies required at runtime.
                for d in p.depends().iter() {
                    if let Some(dix) = PkgGraph::add_dep(db, graph, indices, d.name()) {
                        graph.add_edge(ix, dix, DepType::Hard);
                    }
                }

                ix
            })
        })
    }

    fn add_parent(
        db: &'a Db,
        graph: &mut Graph<&'a str, DepType, Directed, u16>,
        indices: &mut HashMap<&'a str, NodeIndex<u16>>,
        // No lifetime because `required_by` yields owned Strings, which can't
        // have a `'a` lifetime.
        child: &str,
    ) -> Option<NodeIndex<u16>> {
        indices.get(child).cloned().or_else(|| {
            db.pkg(child).ok().map(|p| {
                // We pull the name back out of the summoned `Package` instead
                // of using the given `child: &str` to avoid lifetime issues.
                let name = p.name();
                let ix = graph.add_node(name);
                indices.insert(name, ix);

                for parent in p.required_by() {
                    if let Some(pix) = PkgGraph::add_parent(db, graph, indices, &parent) {
                        graph.add_edge(pix, ix, DepType::Hard);
                    }
                }

                for parent in p.optional_for() {
                    if let Some(pix) = PkgGraph::add_parent(db, graph, indices, &parent) {
                        graph.add_edge(pix, ix, DepType::Opt);
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
            writeln!(
                f,
                "    {} -> {} [{}]",
                e.source().index(),
                e.target().index(),
                e.weight.edge_style()
            )?;
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
