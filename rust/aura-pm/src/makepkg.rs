//! Parsing values from the user's `makepkg.conf`, where ever that happens to
//! be.

use crate::dirs::xdg_config;
use applying::Apply;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::ops::Not;
use std::path::PathBuf;

/// The default filepath of the global Makepkg configuration.
pub(crate) const GLOBAL_MAKEPKG_CONF: &str = "/etc/makepkg.conf";

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
    /// Not a setting per se, but the original path to the config file that was
    /// detected.
    pub(crate) path: PathBuf,
}

impl Makepkg {
    /// Attempt to read certain makepkg settings from the filesystem.
    pub(crate) fn new() -> Result<Self, Error> {
        let path = conf_location();

        let (packager, check) = path
            .as_path()
            .apply(File::open)
            .map_err(Error::Io)?
            .apply(BufReader::new)
            .lines()
            .map_while(Result::ok)
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

        Ok(Makepkg {
            packager,
            check,
            path,
        })
    }
}

fn extract_value(line: &str) -> Option<&str> {
    line.split_once('=').map(|(_, v)| v.trim_matches('"'))
}

fn extract_list(s: &str) -> Vec<&str> {
    s.trim_matches(['(', ')']).split(' ').collect()
}

fn strong_local_conf() -> Option<PathBuf> {
    xdg_config()
        .ok()?
        .join("pacman")
        .join("makepkg.conf")
        .apply(Some)
}

fn weak_local_conf() -> Option<PathBuf> {
    std::env::var("HOME")
        .ok()
        .map(PathBuf::from)?
        .join(".makepkg.conf")
        .apply(Some)
}

/// Rules:
/// 1. Global is at /etc/makepkg.conf by default.
/// 2. The above _global_ config can be overridden via `MAKEPKG_CONF`.
/// 3. If `$XDG_CONFIG_HOME/pacman/makepkg.conf` exists, it will take priority over the global.
/// 4. If `$HOME/.makepkg.conf` exists, but (3) does not, it will take priority over the global.
pub(crate) fn conf_location() -> PathBuf {
    let strong = strong_local_conf();
    let weak = weak_local_conf();

    match (strong, weak) {
        (Some(pb), _) if pb.is_file() => pb,
        (_, Some(pb)) if pb.is_file() => pb,
        (_, _) => std::env::var("MAKEPKG_CONF")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from(GLOBAL_MAKEPKG_CONF)),
    }
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
