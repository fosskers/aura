//! All functionality involving the `-A` command.

use crate::{error::Error, utils};
use chrono::{Date, TimeZone, Utc};
use colored::{ColoredString, Colorize};
use i18n_embed::fluent::FluentLanguageLoader;
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
            ("Repository".to_owned(), "aur".magenta()),
            (fl!(fll, "common-name"), p.name.bold()),
            (fl!(fll, "aur-info-version"), p.version.normal()),
            (
                fl!(fll, "aur-info-status"),
                match p.out_of_date {
                    None => "Up to Date".green(),
                    Some(_) => "Out of Date!".red(),
                },
            ),
            (
                fl!(fll, "aur-info-maintainer"),
                match p.maintainer {
                    None => "None".red(),
                    Some(m) => m.normal(),
                },
            ),
            (
                fl!(fll, "aur-info-project-url"),
                p.url.map(|m| m.cyan()).unwrap_or_else(|| "None".red()),
            ),
            ("AUR URL".to_owned(), package_url(&p.name).normal()),
            (fl!(fll, "aur-info-license"), p.license.join(" ").normal()),
            (fl!(fll, "aur-info-group"), p.groups.join(" ").normal()),
            (fl!(fll, "aur-info-provides"), p.provides.join(" ").normal()),
            (fl!(fll, "aur-info-depends"), p.depends.join(" ").normal()),
            (
                fl!(fll, "aur-info-makedeps"),
                p.make_depends.join(" ").normal(),
            ),
            (
                fl!(fll, "aur-info-optdeps"),
                p.opt_depends.join(" ").normal(),
            ),
            (
                fl!(fll, "aur-info-checkdeps"),
                p.check_depends.join(" ").normal(),
            ),
            (
                fl!(fll, "aur-info-votes"),
                format!("{}", p.num_votes).yellow(),
            ),
            (
                fl!(fll, "aur-info-popularity"),
                format!("{:.2}", p.popularity).yellow(),
            ),
            (
                fl!(fll, "aur-info-description"),
                p.description
                    .map(|d| d.normal())
                    .unwrap_or_else(|| "None".red()),
            ),
            (fl!(fll, "aur-info-keywords"), p.keywords.join(" ").cyan()),
            (
                fl!(fll, "aur-info-submitted"),
                format!("{}", package_date(p.first_submitted).format("%F")).normal(),
            ),
            (
                fl!(fll, "aur-info-updated"),
                format!("{}", package_date(p.last_modified).format("%F")).normal(),
            ),
        ];
        utils::info(&mut w, &pairs)?;
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

fn package_date(epoch: i64) -> Date<Utc> {
    Utc.timestamp(epoch, 0).date()
}
