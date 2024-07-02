//! Parsing values from the user's `makepkg.conf`, where ever that happens to
//! be.

use aura_core::Apply;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::ops::Not;
use std::path::PathBuf;

/// The default filepath of the Makepkg configuration.
pub(crate) const MAKEPKG_CONF: &str = "/etc/makepkg.conf";

pub(crate) enum Error {
    Io(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{e}"),
        }
    }
}

/// A reduced form of the various options and settings available in a
/// `makepkg.conf`.
#[derive(Debug)]
pub(crate) struct Makepkg {
    /// He who packages upon this system.
    pub(crate) packager: Option<String>,
    /// Assumed false if missing from the file.
    pub(crate) check: bool,
}

impl Makepkg {
    /// Attempt to read certain makepkg settings from the filesystem.
    pub(crate) fn new() -> Result<Self, Error> {
        let (packager, check) = conf_location()
            .apply(File::open)
            .map_err(Error::Io)?
            .apply(BufReader::new)
            .lines()
            .filter_map(|line| line.ok())
            .map(|line| line.trim().to_string())
            .filter(|line| line.is_empty().not())
            .filter(|line| line.starts_with('#').not())
            // FIXME 2024-07-01 Generalise this into parsing a `HashMap`.
            //
            // And then extract the fields you need. However, some fields cross
            // more than one line, so the parsing itself isn't entirely
            // straight-forward. For now, I don't want to make a new library
            // just for that.
            .fold((None, false), |(p, c), line| {
                if line.starts_with("PACKAGER=") {
                    (extract_value(&line).map(|s| s.to_string()), c)
                } else if line.starts_with("BUILDENV=") {
                    let list = extract_value(&line).map(extract_list).unwrap_or_default();
                    (p, list.contains(&"check"))
                } else {
                    (p, c)
                }
            });

        Ok(Makepkg { packager, check })
    }
}

fn extract_value(line: &str) -> Option<&str> {
    line.split_once('=').map(|(_, v)| v.trim_matches('"'))
}

fn extract_list(s: &str) -> Vec<&str> {
    s.trim_matches(&['(', ')']).split(' ').collect()
}

pub(crate) fn conf_location() -> PathBuf {
    std::env::var("MAKEPKG_CONF")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(MAKEPKG_CONF))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splitting() {
        let s = "PACKAGER=\"Colin Woodbury <colin@fosskers.ca>\"";
        let r = extract_value(s).unwrap();

        assert_eq!("Colin Woodbury <colin@fosskers.ca>", r);
    }

    #[test]
    fn list() {
        let s = "(!distcc color !ccache check !sign)";
        let r = extract_list(s);
        let e = vec!["!distcc", "color", "!ccache", "check", "!sign"];

        assert_eq!(e, r);
    }
}
