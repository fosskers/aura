//! Core interactions with a [faur](https://git.sr.ht/~fosskers/faur) instance.

use serde::{Deserialize, Serialize};

/// The main `faur` instance.
pub const FAUR_URL: &str = "https://faur.fosskers.ca/";

/// Package information returned from a `faur` instance. Identical in format to
/// the AUR's RPC, but yields empty lists for missing fields.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "PascalCase", deny_unknown_fields)]
pub struct Package {
    check_depends: Vec<String>,
    conflicts: Vec<String>,
    depends: Vec<String>,
    description: Option<String>,
    first_submitted: u64,
    groups: Vec<String>,
    #[serde(rename = "ID")]
    id: u64,
    keywords: Vec<String>,
    last_modified: u64,
    license: Vec<String>,
    maintainer: Option<String>,
    make_depends: Vec<String>,
    name: String,
    num_votes: u64,
    opt_depends: Vec<String>,
    out_of_date: Option<u64>,
    package_base: String,
    #[serde(rename = "PackageBaseID")]
    package_base_id: u64,
    popularity: f64,
    provides: Vec<String>,
    replaces: Vec<String>,
    #[serde(rename = "URL")]
    url: Option<String>,
    #[serde(rename = "URLPath")]
    url_path: String,
    version: String,
}

/// Look up K-many packages by name in a database of N packages: `O(klogn)`
pub fn info<'a, I, F, E>(pkgs: I, fetch: F) -> Result<Vec<Package>, E>
where
    F: Fn(&str) -> Result<Vec<Package>, E>,
    I: Iterator<Item = &'a str>,
{
    // FIXME Thu May  5 21:28:31 2022
    //
    // Once `intersperse` stabilises, use that instead of this wasteful Vec
    // allocation.
    let s: String = pkgs.collect::<Vec<_>>().join(",");
    let url = format!("{}/packages?names={}", FAUR_URL, s);
    fetch(&url)
}

/// Look up packages whose names and/or descriptions contain all of the K-many
/// given terms: `O(klogn)`
pub fn search<'a, I, F, E>(terms: I, fetch: F) -> Result<Vec<Package>, E>
where
    F: Fn(&str) -> Result<Vec<Package>, E>,
    I: Iterator<Item = &'a str>,
{
    // FIXME Thu May  5 21:37:15 2022
    //
    // Same as above.
    let s: String = terms.collect::<Vec<_>>().join(",");
    let url = format!("{}/packages?names={}&by=desc", FAUR_URL, s);
    fetch(&url)
}

/// Look up packages that provide the given "package identity": `O(logn)`
pub fn provides<F, E>(providing: &str, fetch: F) -> Result<Vec<Package>, E>
where
    F: Fn(&str) -> Result<Vec<Package>, E>,
{
    let url = format!("{}/packages?names={}&by=prov", FAUR_URL, providing);
    fetch(&url)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    use std::io::BufReader;

    #[test]
    fn package_parse() {
        let file = File::open("tests/faur.json").unwrap();
        let read = BufReader::new(file);
        let pkgs: Vec<Package> = serde_json::from_reader(read).unwrap();

        assert_eq!(3, pkgs.len());
    }
}
