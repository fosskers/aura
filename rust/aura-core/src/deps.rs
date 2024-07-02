//! Dependency analysis internals.

use crate::DbLike;
use itertools::Itertools;
use log::debug;
use petgraph::graph::NodeIndex;
use petgraph::Directed;
use petgraph::Graph;
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

/// The name of a package group.
type Group<'a> = &'a str;

/// A unique collection of `Package`s and their dependencies.
pub struct PkgGraph<'a> {
    /// The graph itself. Contents are borrowed from an ALPM [`alpm::Db`] to
    /// stay zero-cost.
    graph: Graph<(&'a str, Option<Group<'a>>), DepType, Directed, u16>,
    /// The original nodes around which the graph was built.
    focii: &'a [&'a str],
    /// Foreign packages in the system.
    foreigns: &'a [&'a str],
}

impl<'a> PkgGraph<'a> {
    /// Create a new `PkgGraph` of given packages and all their dependencies.
    pub fn by_deps<D>(
        db: &'a D,
        limit: Option<u8>,
        optional: bool,
        foreigns: &'a [&'a str],
        focii: &'a [&'a str],
    ) -> PkgGraph<'a>
    where
        D: DbLike,
    {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        for p in focii {
            PkgGraph::add_dep(db, &mut graph, &mut indices, limit, optional, p);
        }

        PkgGraph {
            graph,
            focii,
            foreigns,
        }
    }

    /// Create a new `PkgGraph` of given packages and all packages that require them.
    pub fn by_parents<D>(
        db: &'a D,
        limit: Option<u8>,
        optional: bool,
        foreigns: &'a [&'a str],
        focii: &'a [&'a str],
    ) -> PkgGraph<'a>
    where
        D: DbLike,
    {
        let mut graph = Graph::default();
        let mut indices = HashMap::new();

        debug!("Focii: {:?}", focii);

        for p in focii {
            PkgGraph::add_parent(db, &mut graph, &mut indices, limit, optional, p);
        }

        PkgGraph {
            graph,
            focii,
            foreigns,
        }
    }

    /// Recursively add dependencies to the package `Graph`.
    fn add_dep<D>(
        db: &'a D,
        graph: &mut Graph<(&'a str, Option<Group<'a>>), DepType, Directed, u16>,
        indices: &mut HashMap<&'a str, NodeIndex<u16>>,
        limit: Option<u8>,
        optional: bool,
        parent: &'a str,
    ) -> Option<NodeIndex<u16>>
    where
        D: DbLike,
    {
        let true_pkg = db.get_pkg(parent).ok().or_else(|| db.provides(parent))?;
        let name = true_pkg.name();
        debug!("Found {} providing {}", name, parent);

        indices.get(name).cloned().or_else(|| {
            let ix = graph.add_node((name, true_pkg.groups().first()));
            debug!("Added {} at {}", name, ix.index());
            indices.insert(name, ix);
            let next = limit.map(|l| l - 1);

            if next.is_none() || next > Some(0) {
                // Dependencies required at runtime.
                for d in true_pkg.depends().iter() {
                    if let Some(dix) =
                        PkgGraph::add_dep(db, graph, indices, next, optional, d.name())
                    {
                        graph.update_edge(ix, dix, DepType::Hard);
                    }
                }

                if optional {
                    for d in true_pkg.optdepends().iter() {
                        if let Some(dix) =
                            PkgGraph::add_dep(db, graph, indices, next, optional, d.name())
                        {
                            graph.update_edge(ix, dix, DepType::Opt);
                        }
                    }
                }
            }

            Some(ix)
        })
    }

    fn add_parent<D>(
        db: &'a D,
        graph: &mut Graph<(&'a str, Option<Group<'a>>), DepType, Directed, u16>,
        indices: &mut HashMap<&'a str, NodeIndex<u16>>,
        limit: Option<u8>,
        optional: bool,
        // No lifetime because `required_by` yields owned Strings, which can't
        // have a `'a` lifetime.
        child: &str,
    ) -> Option<NodeIndex<u16>>
    where
        D: DbLike,
    {
        let true_pkg = db.get_pkg(child).ok().or_else(|| db.provides(child))?;
        let name = true_pkg.name();
        debug!("Found {} providing {}", name, child);

        indices.get(name).cloned().or_else(|| {
            let ix = graph.add_node((name, true_pkg.groups().first()));
            indices.insert(name, ix);
            let next = limit.map(|l| l - 1);

            if next.is_none() || next > Some(0) {
                for parent in true_pkg.required_by() {
                    if let Some(pix) =
                        PkgGraph::add_parent(db, graph, indices, next, optional, &parent)
                    {
                        graph.update_edge(pix, ix, DepType::Hard);
                    }
                }

                if optional {
                    for parent in true_pkg.optional_for() {
                        if let Some(pix) =
                            PkgGraph::add_parent(db, graph, indices, next, optional, &parent)
                        {
                            graph.update_edge(pix, ix, DepType::Opt);
                        }
                    }
                }
            }

            Some(ix)
        })
    }
}

impl std::fmt::Display for PkgGraph<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "digraph {{")?;
        let graph = &self.graph;

        // Render nodes.
        for (n, (gname, group)) in graph
            .node_indices()
            .filter_map(|ix| graph.node_weight(ix).map(|(name, group)| (ix, name, group)))
            .sorted_by(|a, b| a.2.cmp(b.2))
            .chunk_by(|triple| triple.2)
            .into_iter()
            .enumerate()
        {
            match gname {
                Some(g) => {
                    writeln!(f)?;
                    writeln!(f, "    subgraph cluster_{} {{", n)?;
                    writeln!(f, "        label=\"{}\"", g)?;
                    writeln!(f, "        style=dashed;")?;
                    writeln!(f, "        color=brown;")?;

                    // TODO Deduplicate.
                    for (ix, name, _) in group {
                        let mut styles = vec!["rounded"];

                        if self.focii.contains(name) {
                            styles.push("bold");
                        }

                        if self.foreigns.contains(name) {
                            styles.push("filled");
                        }

                        writeln!(
                            f,
                            "        {} [ label=\"{}\", style=\"{}\", shape=box, fillcolor=\"#4DC6FA\"]",
                            ix.index(),
                            name,
                            styles.join(",")
                        )?;
                    }

                    writeln!(f, "    }}")?;
                }
                None => {
                    for (ix, name, _) in group {
                        let mut styles = vec!["rounded"];

                        if self.focii.contains(name) {
                            styles.push("bold");
                        }

                        if self.foreigns.contains(name) {
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
                }
            }
        }

        writeln!(f)?;

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
