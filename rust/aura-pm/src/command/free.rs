//! License detection.

use applying::Apply;
use r2d2_alpm::Alpm;
use std::collections::HashSet;
use std::ops::Not;

const COPYLEFT: &[&str] = &[
    "GPL", "AGPL", "LGPL", "MPL", "EUPL", "OSL", "EPL", "CPL", "IPL",
];

// References:
//
// - https://www.gnu.org/licenses/license-list.html
// - https://opensource.org/licenses
// - https://en.wikipedia.org/wiki/Comparison_of_free_and_open-source_software_licences#Approvals
//
// Rejected by FSF:
// - NASA
// - RPL (Reciprocal Public License)
//
// Unmentioned by FSF:
// - LiLiQ
// - MirOS
// - Motosoto
// - MulanPSL
// - Multics
// - NGPL (Nethack)
// - NPOSL
// - NTP
// - OGTSL (Open Group testing)
// - OSET
// - PostgreSQL
// - RSCPL
// - SimPL
// - UCL (Upstream)
// - VSL (Vovida)
// - Xnet
//
// Not approved by OSI:
// - Ruby License
// - WTFPL
const FOSS_LICENSES: &[&str] = &[
    "0BSD",
    "AFL", // Academic Free License
    "AGPL",
    "APSL", // Apple
    "Apache",
    "Artistic",
    "BSD",
    "BSL", // Boost
    "CECILL",
    "CPAL",
    "CPL",  // Common Public Licenses
    "ECL",  // Educational Community License
    "EFL",  // Eiffel
    "EPL",  // Eclipse
    "EUPL", // European Union
    "GPL",
    "IPA", // IPA font license
    "ISC", // Internet Systems Consortium
    "IPL", // IBM Public License
    "LGPL",
    "LPPL", // Latex
    "MIT",
    "MPL",   // Mozilla
    "NCSA",  // University of Illinois
    "OFL",   // SIL Open Font License
    "OLDAP", // OpenLDAP
    "OSL",   // Open Software License
    "PHP",
    "RPSL", // RealNetworks Public Source Licenses
    "SPL",  // Sun
    "Sleepycat",
    "UPL", // Universal Permissive License
    "Unlicense",
    "Zlib",
    "eCos",
    "QPL", // Q Public Licenses
    "PSF", // Python
    "NOKIA",
];

pub fn free(alpm: &Alpm, lenient: bool) {
    find_and_print(alpm, lenient, FOSS_LICENSES);
}

pub fn copyleft(alpm: &Alpm, lenient: bool) {
    find_and_print(alpm, lenient, COPYLEFT);
}

fn find_and_print(alpm: &Alpm, lenient: bool, licenses: &[&str]) {
    let lics: HashSet<_> = licenses
        .into_iter()
        .map(|s| s.to_ascii_uppercase())
        .collect();

    for p in alpm.as_ref().localdb().pkgs() {
        let ls: HashSet<_> = p.licenses().iter().flat_map(parse_licenses).collect();

        if ls
            .iter()
            .any(|l| {
                lics.contains(&l.to_ascii_uppercase())
                    || (lenient
                        && split_custom(l)
                            .map(|c| lics.contains(&c.to_ascii_uppercase()))
                            .unwrap_or(false))
            })
            .not()
        {
            let s: String = itertools::intersperse(ls, ", ").collect();
            println!("{}: {}", p.name(), s);
        }
    }
}

fn parse_licenses(raw: &str) -> HashSet<&str> {
    raw.split("AND")
        .flat_map(|s| s.split("OR"))
        .map(parse_prefix)
        .collect()
}

fn parse_prefix(lic: &str) -> &str {
    if lic.starts_with("LicenseRef") {
        lic.trim()
    } else {
        lic.split_once('-')
            .map(|(l, _)| l)
            .unwrap_or(lic)
            .apply(|s| s.trim())
            .apply(trim_numbers)
    }
}

fn trim_numbers(lic: &str) -> &str {
    lic.trim_end_matches(|c: char| c == '.' || c.is_ascii_digit())
}

fn split_custom(lic: &str) -> Option<&str> {
    lic.split_once("custom:").map(|(_, l)| l)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn custom() {
        assert_eq!(Some("BSD"), split_custom("custom:BSD"));
    }

    #[test]
    fn prefix() {
        assert_eq!("MPL", parse_prefix("MPL-1.1"));
        assert_eq!("MPL", parse_prefix("MPL"));
        assert_eq!("GPL", parse_prefix("GPL2.1"));
        assert_eq!("LicenseRef-Java", parse_prefix("LicenseRef-Java"));
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
