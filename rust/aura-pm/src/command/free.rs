//! License detection.

use applying::Apply;
use r2d2_alpm::Alpm;
use std::collections::HashSet;
use std::ops::Not;

const COPYLEFT: &[&str] = &[
    "GPL", "AGPL", "LGPL", "MPL", "EUPL", "OSL", "EPL", "CPL", "IPL",
];

// As found here: https://opensource.org/licenses
const FOSS_LICENSES: &[&str] = &[
    "0BSD",
    "AAL",
    "AFL",
    "AGPL",
    "APSL",
    "Apache",
    "Artistic",
    "BSD",
    "BSL",
    "BlueOak",
    "CAL",
    "CECILL",
    "CPAL",
    "CPL",
    "ECL",
    "EFL",
    "EPL",
    "EUPL",
    "GPL",
    "IPA",
    "ISC",
    "IPL",
    "LGPL",
    "LPPL",
    "LiLiQ",
    "MIT",
    "MPL",
    "MirOS",
    "Motosoto",
    "MulanPSL",
    "Multics",
    "NCSA",
    "NPOSL",
    "NTP",
    "OFL",
    "OLDAP",
    "OSET",
    "OSL",
    "PHP",
    "PostgreSQL",
    "RPSL",
    "RSCPL",
    "SPL",
    "SimPL",
    "Sleepycat",
    "UCL",
    "UPL",
    "Unlicense",
    "VSL",
    "Xnet",
    "Zlib",
    "eCos",
    "QPL",
    "PSF",
    "NGPL",
    "NOKIA",
    "OGTSL",
];

pub fn free(alpm: &Alpm) {
    let lics: HashSet<_> = FOSS_LICENSES.into_iter().map(|s| *s).collect();

    println!("Not using FOSS licenses:");

    for p in alpm.as_ref().localdb().pkgs() {
        let ls: HashSet<_> = p.licenses().iter().flat_map(parse_licenses).collect();

        if ls.iter().any(|l| lics.contains(l)).not() {
            println!("{}: {:?}", p.name(), ls);
        }
    }
}

pub fn copyleft(alpm: &Alpm) {
    let lics: HashSet<_> = COPYLEFT.into_iter().map(|s| *s).collect();

    println!("Not using Copyleft licenses:");

    for p in alpm.as_ref().localdb().pkgs() {
        let ls: HashSet<_> = p.licenses().iter().flat_map(parse_licenses).collect();

        if ls.iter().any(|l| lics.contains(l)).not() {
            println!("{}: {:?}", p.name(), ls);
        }
    }
}

// /// All licenses currently associated with installed packages.
// fn licenses(alpm: &Alpm) -> HashSet<&str> {
//     alpm.as_ref()
//         .localdb()
//         .pkgs()
//         .iter()
//         .flat_map(|p| p.licenses())
//         .collect()
// }

fn parse_licenses(raw: &str) -> HashSet<&str> {
    raw.split("AND")
        .flat_map(|s| s.split("OR"))
        .map(parse_prefix)
        .collect()
}

fn parse_prefix(lic: &str) -> &str {
    lic.split_once('-')
        .map(|(l, _)| l)
        .unwrap_or(lic)
        .apply(|s| s.trim())
        .apply(trim_numbers)
}

fn trim_numbers(lic: &str) -> &str {
    lic.trim_end_matches(|c: char| c == '.' || c.is_ascii_digit())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefix() {
        assert_eq!("MPL", parse_prefix("MPL-1.1"));
        assert_eq!("MPL", parse_prefix("MPL"));
        assert_eq!("GPL", parse_prefix("GPL2.1"));
    }

    #[test]
    fn parsing() {
        let mut set = HashSet::new();
        set.insert("MPL");
        set.insert("LGPL");
        assert_eq!(set, parse_licenses("MPL-1.1 OR LGPL-2.1-only"));

        let mut set = HashSet::new();
        set.insert("MIT");
        assert_eq!(set, parse_licenses("MIT"));

        let mut set = HashSet::new();
        set.insert("MIT");
        set.insert("BSD");
        set.insert("SGI");
        assert_eq!(set, parse_licenses("MIT AND BSD-3-Clause AND SGI-B-2.0"));
    }
}
