//! Core interactions with a [faur](https://git.sr.ht/~fosskers/faur) instance.

use serde::{Deserialize, Serialize};

/// The main `faur` instance.
pub const FAUR_URL: &str = "https://faur.fosskers.ca";

/// Package information returned from a `faur` instance. Identical in format to
/// the AUR's RPC, but yields empty lists for missing fields.
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "PascalCase")]
pub struct Package {
    /// Dependencies only necessary for testing.
    #[serde(default)]
    pub check_depends: Vec<String>,
    /// Packages which cannot be installed at the same time as this one.
    #[serde(default)]
    pub conflicts: Vec<String>,
    /// Runtime dependencies.
    #[serde(default)]
    pub depends: Vec<String>,
    /// The general description of the package.
    pub description: Option<String>,
    /// Timestamp of the first uploading of this package to the AUR.
    pub first_submitted: u64,
    /// Package groups this package belongs to.
    #[serde(default)]
    pub groups: Vec<String>,
    /// An internal identifier.
    #[serde(rename = "ID")]
    pub id: u64,
    /// General categories this package belongs to.
    #[serde(default)]
    pub keywords: Vec<String>,
    /// Timestamp of the latest upload of this package.
    pub last_modified: u64,
    /// The software LICENSE in use by this package.
    pub license: Vec<String>,
    /// The maintainer of the AUR package.
    pub maintainer: Option<String>,
    /// Dependencies only necessary at build time.
    #[serde(default)]
    pub make_depends: Vec<String>,
    /// The normal name of this package.
    pub name: String,
    /// The number of votes received on the AUR.
    pub num_votes: u64,
    /// Optional runtime depedencies.
    #[serde(default)]
    pub opt_depends: Vec<String>,
    /// Timestamp of an Out-of-date report on the AUR.
    pub out_of_date: Option<u64>,
    /// In the case of split packages, this is the "parent". Otherwise, this is
    /// equal to `name`.
    pub package_base: String,
    /// Internal identifier of the parent package. Equal to `id` if this package
    /// isn't part of a split package set.
    #[serde(rename = "PackageBaseID")]
    pub package_base_id: u64,
    /// The value of votes wanes over time. This shows how "hot" the package
    /// currently is.
    pub popularity: f64,
    /// Package names that this package "counts as" if installed. For instance,
    /// `aura-bin` "counts as" `aura` for the purpose of dependency resolution.
    #[serde(default)]
    pub provides: Vec<String>,
    /// If package is installed, the packages named in `replaces` should be
    /// uninstalled.
    #[serde(default)]
    pub replaces: Vec<String>,
    /// The URL of the original project.
    #[serde(rename = "URL")]
    pub url: Option<String>,
    /// The URL to the AUR's listing of this package.
    #[serde(rename = "URLPath")]
    pub url_path: String,
    /// The current version of the package.
    pub version: String,
}

/// Look up K-many packages by name in a database of N packages: `O(klogn)`
pub fn info<'a, I, F, E>(pkgs: I, fetch: &F) -> Result<Vec<Package>, E>
where
    F: Fn(&str) -> Result<Vec<Package>, E>,
    I: IntoIterator<Item = &'a str>,
{
    // FIXME Thu May  5 21:28:31 2022
    //
    // Once `intersperse` stabilises, use that instead of this wasteful Vec
    // allocation.
    let s: String = pkgs.into_iter().collect::<Vec<_>>().join(",");
    let url = format!("{}/packages?names={}", FAUR_URL, s);
    fetch(&url)
}

/// Look up packages whose names and/or descriptions contain all of the K-many
/// given terms: `O(klogn)`
pub fn search<'a, I, F, E>(terms: I, fetch: &F) -> Result<Vec<Package>, E>
where
    F: Fn(&str) -> Result<Vec<Package>, E>,
    I: IntoIterator<Item = &'a str>,
{
    // FIXME Thu May  5 21:37:15 2022
    //
    // Same as above.
    let s: String = terms.into_iter().collect::<Vec<_>>().join(",");
    let url = format!("{}/packages?names={}&by=desc", FAUR_URL, s);
    fetch(&url)
}

/// Look up packages that provide the given "package identity": `O(logn)`
pub fn provides<F, E>(providing: &str, fetch: &F) -> Result<Vec<Package>, E>
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
