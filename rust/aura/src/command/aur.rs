//! All functionality involving the `-A` command.

mod build;

use crate::utils::ResultVoid;
use crate::{aura, dirs, green, red, yellow};
use alpm::Alpm;
use aura_core::aur::PkgPartition;
use chrono::{TimeZone, Utc};
use colored::{ColoredString, Colorize};
use from_variants::FromVariants;
use i18n_embed::{fluent::FluentLanguageLoader, LanguageLoader};
use i18n_embed_fl::fl;
use log::{debug, info};
use nonempty::NonEmpty;
use r2d2::Pool;
use r2d2_alpm::AlpmManager;
use rayon::prelude::*;
use std::borrow::{Borrow, Cow};
use std::io::{BufWriter, Write};
use std::ops::Not;
use std::path::{Path, PathBuf};
use validated::Validated;

#[derive(FromVariants)]
pub enum Error {
    Fetch(crate::fetch::Error),
    Dirs(crate::dirs::Error),
    Io(std::io::Error),
    Git(aura_core::git::Error),
    #[from_variants(skip)]
    Clones(NonEmpty<aura_core::git::Error>),
    #[from_variants(skip)]
    Pulls(NonEmpty<aura_core::git::Error>),
    Build(build::Error),
    Deps(aura_core::aur::dependencies::Error<crate::fetch::Error>),
    Pacman(crate::pacman::Error),
    R2d2(r2d2::Error),
    Cancelled,
    Silent,
}

// TODO Thu Jan 20 15:32:13 2022
//
// Eventually these will all be deleted when error messages are localised.
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Dirs(e) => write!(f, "{}", e),
            Error::Io(e) => write!(f, "{}", e),
            Error::Git(e) => write!(f, "{}", e),
            Error::Silent => write!(f, ""),
            Error::Clones(es) => {
                for e in es {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
            Error::Pulls(es) => {
                for e in es {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
            Error::Build(e) => write!(f, "{}", e),
            Error::Pacman(e) => write!(f, "{}", e),
            Error::Deps(e) => write!(f, "{}", e),
            Error::R2d2(e) => write!(f, "{}", e),
            Error::Fetch(e) => write!(f, "{}", e),
            Error::Cancelled => write!(f, "Installation cancelled."),
        }
    }
}

/// View AUR package information.
pub(crate) fn info(fll: &FluentLanguageLoader, packages: &[String]) -> Result<(), Error> {
    info!("-Ai on {:?}", packages);
    let r: Vec<aura_core::faur::Package> = aura_core::faur::info(
        packages.iter().map(|s| s.as_str()),
        &crate::fetch::fetch_json,
    )?;
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
            (
                &name,
                if p.name == p.package_base {
                    p.name.bold()
                } else {
                    format!("{} ({})", p.name.bold(), p.package_base.cyan()).normal()
                },
            ),
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
        crate::utils::info(&mut w, fll.current_language(), &pairs)?;
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

    let mut matches: Vec<aura_core::faur::Package> =
        aura_core::faur::search(terms.iter().map(|s| s.as_str()), &crate::fetch::fetch_json)?;

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

/// Open a given package's AUR package in a browser.
pub(crate) fn open(package: &str) -> Result<(), std::io::Error> {
    let url = package_url(package);
    crate::open::open(&url)
}

/// A package's URL on the AUR.
fn package_url(package: &str) -> String {
    format!("{}{}", crate::open::AUR_PKG_URL, package)
}

fn package_date(epoch: u64) -> ColoredString {
    // FIXME Thu May  5 22:11:40 2022
    //
    // There is a panic risk here with the u64->i64 conversion. In practice it
    // should never come up, as the timestamps passed in should never be
    // anywhere near the [`u64::MAX`] value.
    format!("{}", Utc.timestamp(epoch as i64, 0).date().format("%F")).normal()
}

/// Clone the AUR repository of given packages.
pub(crate) fn clone_aur_repos(
    fll: &FluentLanguageLoader,
    packages: &[String],
) -> Result<(), Error> {
    let clones: Validated<(), &str> = packages
        .par_iter()
        .map(|p| {
            let pkg = p.as_str();
            aura!(fll, "A-w", package = pkg);
            aura_core::aur::clone_aur_repo(None, &p)
                .map_err(|_| pkg)
                .void()
        })
        .collect();

    match clones {
        Validated::Good(_) => {
            green!(fll, "common-done");
            Ok(())
        }
        Validated::Fail(bads) => {
            red!(fll, "A-w-fail");

            for bad in bads {
                eprintln!("  - {}", bad);
            }

            Err(Error::Silent)
        }
    }
}

// TODO Add a progress bar here.
/// Pull the latest commits from every clone in the `packages` directory.
pub(crate) fn refresh(fll: &FluentLanguageLoader) -> Result<(), Error> {
    let pulls: Validated<(), String> = dirs::clones()?
        .read_dir()?
        .filter_map(|rde| rde.ok())
        .filter(|de| de.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
        .par_bridge()
        .filter_map(|de| de.file_name().into_string().ok().map(|p| (p, de.path())))
        .map(|(pkg, path)| aura_core::git::pull(&path).map_err(|_| pkg.clone()))
        .collect();

    // FIXME Fri Jan 28 13:07:24 2022
    //
    // Return the failed packages in the Error type instead?
    match pulls {
        Validated::Good(_) => {
            green!(fll, "common-done");
            Ok(())
        }
        Validated::Fail(bads) => {
            red!(fll, "A-y");

            for bad in bads {
                eprintln!("  - {}", bad);
            }

            Err(Error::Silent)
        }
    }
}

// TODO Thu Jan 13 17:41:55 2022
//
// This will obviously require more arguments.
pub(crate) fn install(
    fll: &FluentLanguageLoader,
    config: pacmanconf::Config,
    pkgs: &[String],
) -> Result<(), Error> {
    // Exit early if the user passed no packages.
    if pkgs.is_empty() {
        red!(fll, "common-no-packages");
        return Err(Error::Silent);
    }

    let clone_dir = crate::dirs::clones()?;
    let build_dir = crate::dirs::builds()?;
    let cache_dir = crate::dirs::tarballs()?;

    let mngr = AlpmManager::new(config);
    // FIXME Fri Feb  4 15:58:38 2022
    //
    // Set `max_size` based on the user's CPU count.
    let pool = Pool::builder().max_size(4).build(mngr)?;
    aura!(fll, "A-install-deps");
    let rslv =
        aura_core::aur::dependencies::resolve(pool, &crate::fetch::fetch_json, &clone_dir, pkgs)?;

    debug!("Satisfied: {:?}", rslv.satisfied);
    debug!("To install: {:?}", rslv.to_install);
    debug!("To build: {:?}", rslv.to_build);

    // --- Sort package names alphabetically --- //
    let to_install = {
        let mut v: Vec<_> = rslv.to_install.into_iter().collect();
        v.sort();
        v
    };
    let to_build = {
        let mut v: Vec<_> = rslv.to_build.into_iter().collect();
        v.sort_by(|a, b| a.name.cmp(&b.name));
        v
    };

    if to_install.is_empty().not() {
        aura!(fll, "A-install-repo-pkgs");
        to_install.iter().for_each(|p| println!(" {p}"));
    }
    aura!(fll, "A-install-aur-pkgs");
    to_build.iter().for_each(|p| println!(" {p}"));

    crate::utils::proceed(fll)
        .then(|| ())
        .ok_or(Error::Cancelled)?;

    // --- Determine the best build order --- //
    let order: Vec<Vec<&str>> = aura_core::aur::dependencies::build_order(&to_build)?;
    info!("Build order: {:?}", order);

    // --- Install repo dependencies --- //
    crate::pacman::pacman_install_from_repos(
        std::iter::once("--asdeps")
            .chain(std::iter::once("--noconfirm"))
            .chain(to_install.iter().map(|o| o.borrow())),
    )?;

    // --- Build and install each layer of AUR packages --- //
    for layer in order {
        let clone_paths = layer
            .into_iter()
            .map(|pkg| [&clone_dir, Path::new(pkg)].iter().collect::<PathBuf>());

        let tarballs = build::build(fll, &cache_dir, &build_dir, clone_paths)?;
        crate::pacman::pacman_install_from_tarball(tarballs)?;
    }

    Ok(())
}

fn real_packages<'a>(
    fll: &FluentLanguageLoader,
    pkgs: &'a [String],
) -> Result<(Vec<Cow<'a, str>>, Vec<Cow<'a, str>>), Error> {
    let clone_dir = crate::dirs::clones()?;
    let PkgPartition {
        cloned,
        to_clone,
        not_real,
    } = aura_core::aur::partition_aur_pkgs(&crate::fetch::fetch_json, &clone_dir, pkgs)?;

    if cloned.is_empty() && to_clone.is_empty() {
        red!(fll, "common-no-valid");
        return Err(Error::Silent);
    }

    for bad in not_real {
        let name = bad.cyan().to_string();
        yellow!(fll, "A-install-unreal", pkg = name);
    }

    Ok((cloned, to_clone))
}
