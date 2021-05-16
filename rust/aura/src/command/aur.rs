//! All functionality involving the `-A` command.

use crate::{error::Error, utils};
use colored::Colorize;
use raur_curl::{Handle, Raur};
use std::borrow::Cow;
use std::io::BufWriter;

/// View AUR package information.
pub(crate) fn info(packages: &[String]) -> Result<(), Error> {
    let h = Handle::new();
    let r = h.info(packages)?;
    let mut w = BufWriter::new(std::io::stdout());

    for p in r {
        let pairs = vec![
            ("Repository", "aur".magenta()),
            ("Name", p.name.bold()),
            ("Version", p.version.normal()),
            (
                "AUR Status",
                match p.out_of_date {
                    None => "Up to Date".green(),
                    Some(_) => "Out of Date!".red(),
                },
            ),
            (
                "Maintainer",
                match p.maintainer {
                    None => "None".red(),
                    Some(m) => m.normal(),
                },
            ),
            (
                "Project URL",
                p.url.map(|m| m.cyan()).unwrap_or_else(|| "None".red()),
            ),
            // ("AUR URL", p..normal()),
            ("License", p.license.join(" ").normal()),
            ("Depends On", p.depends.join(" ").normal()),
            ("Make Deps", p.make_depends.join(" ").normal()),
            ("Check Deps", p.check_depends.join(" ").normal()),
            ("Votes", format!("{}", p.num_votes).yellow()),
            ("Popularity", format!("{:.2}", p.popularity).yellow()),
            (
                "Description",
                p.description
                    .map(|d| d.normal())
                    .unwrap_or_else(|| "None".red()),
            ),
        ];
        utils::info(&mut w, &pairs)?;
    }

    Ok(())
}

/// Open a given package's AUR package.
pub(crate) fn open(package: &str) -> Result<(), Error> {
    let mut url = Cow::from(crate::open::AUR_URL);
    url += package;
    crate::open::open(&url)
}
