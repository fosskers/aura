//! All functionality involving the `-A` command.

use crate::error::Error;
use crate::utils::{self, ResultVoid};
use crate::{aln, aura, dirs, green, red};
use alpm::Alpm;
use aura_core::fp::Validated;
use chrono::{TimeZone, Utc};
use colored::{ColoredString, Colorize};
use i18n_embed::{fluent::FluentLanguageLoader, LanguageLoader};
use i18n_embed_fl::fl;
use raur_curl::{Handle, Raur};
use rayon::prelude::*;
use std::borrow::Cow;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

/// The base path of the URL.
pub const AUR_BASE_URL: &str = "https://aur.archlinux.org/";

/// View AUR package information.
pub(crate) fn info(fll: &FluentLanguageLoader, packages: &[String]) -> Result<(), Error> {
    let h = Handle::new();
    let r = h.info(packages)?;
    let mut w = BufWriter::new(std::io::stdout());

    let repo = fl!(fll, "A-i-repo");
    let name = fl!(fll, "common-name");
    let ver = fl!(fll, "A-i-version");
    let stat = fl!(fll, "A-i-status");
    let main = fl!(fll, "A-i-maintainer");
    let proj = fl!(fll, "A-i-proj-url");
    let aur = fl!(fll, "A-i-aur-url");
    let lic = fl!(fll, "A-i-license");
    let grp = fl!(fll, "A-i-group");
    let prov = fl!(fll, "A-i-provides");
    let deps = fl!(fll, "A-i-depends");
    let make = fl!(fll, "A-i-make");
    let opts = fl!(fll, "A-i-opt");
    let check = fl!(fll, "A-i-check");
    let votes = fl!(fll, "A-i-votes");
    let pop = fl!(fll, "A-i-pop");
    let desc = fl!(fll, "A-i-desc");
    let keys = fl!(fll, "A-i-keywords");
    let sub = fl!(fll, "A-i-submitted");
    let upd = fl!(fll, "A-i-updated");

    for p in r {
        let pairs: Vec<(&str, ColoredString)> = vec![
            (&repo, "aur".magenta()),
            (&name, p.name.bold()),
            (&ver, p.version.normal()),
            (
                &stat,
                match p.out_of_date {
                    None => "Up to Date".green(),
                    Some(_) => "Out of Date!".red(),
                },
            ),
            (
                &main,
                match p.maintainer {
                    None => "None".red(),
                    Some(m) => m.normal(),
                },
            ),
            (
                &proj,
                p.url.map(|m| m.cyan()).unwrap_or_else(|| "None".red()),
            ),
            (&aur, package_url(&p.name).normal()),
            (&lic, p.license.join(" ").normal()),
            (&grp, p.groups.join(" ").normal()),
            (&prov, p.provides.join(" ").normal()),
            (&deps, p.depends.join(" ").normal()),
            (&make, p.make_depends.join(" ").normal()),
            (&opts, p.opt_depends.join(" ").normal()),
            (&check, p.check_depends.join(" ").normal()),
            (&votes, format!("{}", p.num_votes).yellow()),
            (&pop, format!("{:.2}", p.popularity).yellow()),
            (
                &desc,
                p.description
                    .map(|d| d.normal())
                    .unwrap_or_else(|| "None".red()),
            ),
            (&keys, p.keywords.join(" ").cyan()),
            (&sub, package_date(p.first_submitted)),
            (&upd, package_date(p.last_modified)),
        ];
        utils::info(&mut w, fll.current_language(), &pairs)?;
        writeln!(w)?;
    }

    Ok(())
}

/// Search the AUR via a search string.
///
/// Thanks to `clap`, the `terms` slice is guaranteed to be non-empty.
pub(crate) fn search(
    alpm: &Alpm,
    alpha: bool,
    rev: bool,
    limit: Option<usize>,
    quiet: bool,
    mut terms: Vec<String>,
) -> Result<(), Error> {
    let db = alpm.localdb();
    let rep = "aur/".magenta();

    // Sanitize the input.
    terms.sort_unstable_by_key(|t| t.len());
    for t in terms.iter_mut() {
        t.make_ascii_lowercase();
    }

    // Search using the largest term.
    let initial_term = terms.pop().unwrap();
    let mut matches: Vec<_> = Handle::new().search(initial_term)?;

    // Filter out packages that don't match other search terms.
    matches.retain(|m| {
        let name = m.name.to_lowercase();
        let description = m
            .description
            .as_deref()
            .map(|s| s.to_lowercase())
            .unwrap_or_default();
        terms
            .iter()
            .all(|t| name.contains(t) | description.contains(t))
    });

    // Sort and filter the results as requested.
    if alpha {
        matches.sort_by(|a, b| a.name.cmp(&b.name));
    } else {
        matches.sort_by(|a, b| b.num_votes.cmp(&a.num_votes));
    }
    if rev {
        matches.reverse();
    }
    let to_take = limit.unwrap_or_else(|| matches.len());

    for p in matches.into_iter().take(to_take) {
        if quiet {
            println!("{}", p.name);
        } else {
            let n = p.name.bold();
            let vot = format!("{}", p.num_votes).yellow();
            let pop = format!("{:.2}", p.popularity).yellow();
            let ver = match p.out_of_date {
                Some(_) => p.version.red(),
                None => p.version.green(),
            };
            let ins = match db.pkg(p.name) {
                Err(_) => "".normal(),
                Ok(_) => "[installed]".bold(),
            };

            // TODO Search term highlighting
            println!("{}{} {} ({} | {}) {}", rep, n, ver, vot, pop, ins);
            println!("    {}", p.description.unwrap_or_default());
        }
    }

    Ok(())
}

/// Open a given package's AUR package.
pub(crate) fn open(package: &str) -> Result<(), std::io::Error> {
    let url = package_url(package);
    crate::open::open(&url)
}

/// A package's URL on the AUR.
fn package_url(package: &str) -> String {
    let mut url = Cow::from(crate::open::AUR_PKG_URL);
    url += package;
    url.into_owned()
}

fn package_date(epoch: i64) -> ColoredString {
    format!("{}", Utc.timestamp(epoch, 0).date().format("%F")).normal()
}

/// Clone the AUR repository of given packages.
pub(crate) fn clone_repos(fll: &FluentLanguageLoader, packages: &[String]) -> Result<(), Error> {
    let clones: Validated<(), &str> = packages
        .par_iter()
        .map(|p| {
            let pkg = p.as_str();
            aura!(fll, "A-w", package = pkg);
            clone_aur_repo(None, &p).ok_or(pkg).void()
        })
        .collect();

    match clones {
        Validated::Success(_) => {
            green!(fll, "common-done");
            Ok(())
        }
        Validated::Failure(bads) => {
            red!(fll, "A-w-fail");

            for bad in bads {
                eprintln!("  - {}", bad);
            }

            Err(Error::Silent)
        }
    }
}

/// Pull the latest commits from every clone in the `packages` directory.
pub(crate) fn refresh(fll: &FluentLanguageLoader) -> Result<(), Error> {
    let pulls: Validated<(), String> = dirs::clones()?
        .read_dir()?
        .filter_map(|rde| rde.ok())
        .filter(|de| de.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
        .par_bridge()
        .filter_map(|de| de.file_name().into_string().ok().map(|p| (p, de.path())))
        .map(|(pkg, path)| {
            aura_core::git::pull(&path)
                .map_err(|_| pkg.clone())
                .and_then(|status| status.success().then(|| ()).ok_or(pkg))
        })
        .collect();

    match pulls {
        Validated::Success(_) => {
            green!(fll, "common-done");
            Ok(())
        }
        Validated::Failure(bads) => {
            red!(fll, "A-y");

            for bad in bads {
                eprintln!("  - {}", bad);
            }

            Err(Error::Silent)
        }
    }
}

/// Clone a package's AUR repository and return the full path to the clone.
fn clone_aur_repo(root: Option<&Path>, package: &str) -> Option<PathBuf> {
    let mut url: PathBuf = [AUR_BASE_URL, package].iter().collect();
    url.set_extension("git");

    let clone_path: PathBuf = match root {
        None => PathBuf::from(package),
        Some(r) => [r, Path::new(package)].iter().collect(),
    };

    aura_core::git::shallow_clone(&url, &clone_path)
        .ok()
        .and_then(|status| status.success().then(|| clone_path))
}

/// Quickly check some given packages names against the local cache of package
/// clones to see if the arguments are real AUR packages.
fn is_aur_package_fast(clone_dir: &Path, package: &str) -> bool {
    let mut path = clone_dir.to_path_buf();
    path.push(package);
    path.is_dir()
}
