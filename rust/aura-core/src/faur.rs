//! Core interactions with a [faur](https://git.sr.ht/~fosskers/faur) instance.

use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "PascalCase", deny_unknown_fields)]
struct Package {
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
