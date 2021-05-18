//! All functionality involving the `-A` command.

use crate::{error::Error, utils};
use alpm::Alpm;
use chrono::{TimeZone, Utc};
use colored::{ColoredString, Colorize};
use i18n_embed::{fluent::FluentLanguageLoader, LanguageLoader};
use i18n_embed_fl::fl;
use raur_curl::{Handle, Raur};
use std::borrow::Cow;
use std::io::{BufWriter, Write};

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

// TODO Do a search per-term and then combine the results.
/// Search the AUR via a search string.
pub(crate) fn search(alpm: &Alpm, terms: &[String]) -> Result<(), Error> {
    let db = alpm.localdb();
    let h = Handle::new();
    let rep = "aur/".magenta();
    let mut r: Vec<_> = h.search(terms.join(" "))?;
    r.sort_by_key(|p| p.num_votes);
    r.reverse();

    for p in r {
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

        // TODO Sorting schemes
        // TODO Search term highlighting
        println!("{}{} {} ({} | {}) {}", rep, n, ver, vot, pop, ins);
        println!("    {}", p.description.unwrap_or_else(|| "".to_owned()));
    }

    Ok(())
}

/// Open a given package's AUR package.
pub(crate) fn open(package: &str) -> Result<(), Error> {
    let url = package_url(package);
    crate::open::open(&url)
}

/// A package's URL on the AUR.
fn package_url(package: &str) -> String {
    let mut url = Cow::from(crate::open::AUR_URL);
    url += package;
    url.into_owned()
}

fn package_date(epoch: i64) -> ColoredString {
    format!("{}", Utc.timestamp(epoch, 0).date().format("%F")).normal()
}
