//! All functionality involving the `-A` command.

use crate::{error::Error, utils};
use chrono::{TimeZone, Utc};
use colored::{ColoredString, Colorize};
use i18n_embed::{fluent::FluentLanguageLoader, LanguageLoader};
use i18n_embed_fl::fl;
use raur_curl::{Handle, Raur};
use std::borrow::Cow;
use std::io::BufWriter;

/// View AUR package information.
pub(crate) fn info(fll: &FluentLanguageLoader, packages: &[String]) -> Result<(), Error> {
    let h = Handle::new();
    let r = h.info(packages)?;
    let mut w = BufWriter::new(std::io::stdout());

    for p in r {
        let pairs: Vec<(String, ColoredString)> = vec![
            (fl!(fll, "A-i-repo"), "aur".magenta()),
            (fl!(fll, "common-name"), p.name.bold()),
            (fl!(fll, "A-i-version"), p.version.normal()),
            (
                fl!(fll, "A-i-status"),
                match p.out_of_date {
                    None => "Up to Date".green(),
                    Some(_) => "Out of Date!".red(),
                },
            ),
            (
                fl!(fll, "A-i-maintainer"),
                match p.maintainer {
                    None => "None".red(),
                    Some(m) => m.normal(),
                },
            ),
            (
                fl!(fll, "A-i-proj-url"),
                p.url.map(|m| m.cyan()).unwrap_or_else(|| "None".red()),
            ),
            (fl!(fll, "A-i-aur-url"), package_url(&p.name).normal()),
            (fl!(fll, "A-i-license"), p.license.join(" ").normal()),
            (fl!(fll, "A-i-group"), p.groups.join(" ").normal()),
            (fl!(fll, "A-i-provides"), p.provides.join(" ").normal()),
            (fl!(fll, "A-i-depends"), p.depends.join(" ").normal()),
            (fl!(fll, "A-i-make"), p.make_depends.join(" ").normal()),
            (fl!(fll, "A-i-opt"), p.opt_depends.join(" ").normal()),
            (fl!(fll, "A-i-check"), p.check_depends.join(" ").normal()),
            (fl!(fll, "A-i-votes"), format!("{}", p.num_votes).yellow()),
            (fl!(fll, "A-i-pop"), format!("{:.2}", p.popularity).yellow()),
            (
                fl!(fll, "A-i-desc"),
                p.description
                    .map(|d| d.normal())
                    .unwrap_or_else(|| "None".red()),
            ),
            (fl!(fll, "A-i-keywords"), p.keywords.join(" ").cyan()),
            (fl!(fll, "A-i-submitted"), package_date(p.first_submitted)),
            (fl!(fll, "A-i-updated"), package_date(p.last_modified)),
        ];
        utils::info(&mut w, fll.current_language(), &pairs)?;
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
