//! Dependency analysis internals.

use alpm::Db;
use petgraph::{graph::NodeIndex, Directed, Graph};
use std::collections::HashMap;

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
pub struct PkgGraph<'a> {
    /// The graph itself. Contents are borrowed from an ALPM [`alpm::Db`] to
    /// stay zero-cost.
    graph: Graph<&'a str, DepType, Directed, u16>,
    /// The original nodes around which the graph was built.
    focii: &'a [&'a str],
    /// Foreign packages in the system.
    foreigns: &'a [&'a str],
}

impl<'a> PkgGraph<'a> {
    /// Create a new `PkgGraph` of given packages and all their dependencies.
    pub fn by_deps(
        db: &'a Db,
        limit: Option<u8>,
        optional: bool,
        foreigns: &'a [&'a str],
        focii: &'a [&'a str],
    ) -> Result<PkgGraph<'a>, alpm::Error> {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        for p in focii {
            PkgGraph::add_dep(db, &mut graph, &mut indices, limit, optional, p);
        }

        Ok(PkgGraph {
            graph,
            focii,
            foreigns,
        })
    }

    /// Create a new `PkgGraph` of given packages and all packages that require them.
    pub fn by_parents(
        db: &'a Db,
        limit: Option<u8>,
        optional: bool,
        foreigns: &'a [&'a str],
        focii: &'a [&'a str],
    ) -> Result<PkgGraph<'a>, alpm::Error> {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        for p in focii {
            PkgGraph::add_parent(db, &mut graph, &mut indices, limit, optional, p);
        }

        Ok(PkgGraph {
            graph,
            focii,
            foreigns,
        })
    }

    /// Recursively add dependencies to the package `Graph`.
    fn add_dep(
        db: &'a Db,
        graph: &mut Graph<&'a str, DepType, Directed, u16>,
        indices: &mut HashMap<&'a str, NodeIndex<u16>>,
        limit: Option<u8>,
        optional: bool,
        parent: &'a str,
    ) -> Option<NodeIndex<u16>> {
        indices.get(parent).cloned().or_else(|| {
            db.pkg(parent).ok().map(|p| {
                let ix = graph.add_node(parent);
                indices.insert(parent, ix);
                let next = limit.map(|l| l - 1);

                if next.is_none() || next > Some(0) {
                    // Dependencies required at runtime.
                    for d in p.depends().iter() {
                        if let Some(dix) =
                            PkgGraph::add_dep(db, graph, indices, next, optional, d.name())
                        {
                            graph.add_edge(ix, dix, DepType::Hard);
                        }
                    }

                    if optional {
                        for d in p.optdepends().iter() {
                            if let Some(dix) =
                                PkgGraph::add_dep(db, graph, indices, next, optional, d.name())
                            {
                                graph.add_edge(ix, dix, DepType::Opt);
                            }
                        }
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
        limit: Option<u8>,
        optional: bool,
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
                let next = limit.map(|l| l - 1);

                if next.is_none() || next > Some(0) {
                    for parent in p.required_by() {
                        if let Some(pix) =
                            PkgGraph::add_parent(db, graph, indices, next, optional, &parent)
                        {
                            graph.add_edge(pix, ix, DepType::Hard);
                        }
                    }

                    if optional {
                        for parent in p.optional_for() {
                            if let Some(pix) =
                                PkgGraph::add_parent(db, graph, indices, next, optional, &parent)
                            {
                                graph.add_edge(pix, ix, DepType::Opt);
                            }
                        }
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
        let graph = &self.graph;

        // Render nodes.
        for (ix, name) in graph.node_indices().map(|ix| (ix, graph[ix])) {
            let mut styles = vec!["rounded"];

            if self.focii.contains(&name) {
                styles.push("bold");
            }

            if self.foreigns.contains(&name) {
                styles.push("filled");
            }

            writeln!(
                f,
                "    {} [ label=\"{}\", style=\"{}\", shape=box, fillcolor=\"#4DC6FA\"]",
                ix.index(),
                name,
                styles.join(",")
            )?;
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
