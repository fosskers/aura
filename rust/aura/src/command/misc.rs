//! Miscellaneous functionality.

use crate::error::Error;
use crate::flags::Conf;
use std::path::Path;
use std::process::Command;

/// Expected location of the `bat` executable if installed from official repos.
const BAT: &str = "/bin/bat";

/// Expected location of the `less` executable.
const LESS: &str = "/bin/less";

/// Expected location of the `ripgrep` executable.
const RIPGREP: &str = "/bin/rg";

/// Expected location of the `grep` executable.
const GREP: &str = "/bin/grep";

// TODO Move to `conf` module.
/// Open the `pacman.conf` in `bat` or `less`.
pub fn pacman_conf(c: Conf) -> Result<(), Error> {
    let conf = c.config.unwrap_or(aura_arch::DEFAULT_PAC_CONF.to_string());
    let prog = viewer();
    Command::new(prog).arg(conf).status().map_err(Error::IO)?;
    Ok(())
}

// TODO Move to `conf` module.
// Check `MAKEPKG_CONF` env var.
/// Open the `makepkg.conf` in `bat` or `less`.
pub fn makepkg_conf() -> Result<(), Error> {
    let conf = aura_arch::DEFAULT_MAKEPKG_CONF;
    let prog = viewer();
    Command::new(prog).arg(conf).status().map_err(Error::IO)?;
    Ok(())
}

/// Display the locales that Aura has been translated to.
pub fn languages() {
    for lang in crate::localization::available_languages() {
        println!("{}", lang);
    }
}

/// A complete path to a file viewer program like `less`.
pub fn viewer() -> &'static str {
    let bat = Path::new(BAT);
    let viewer = if bat.exists() { BAT } else { LESS };
    viewer
}

/// A complete path to a file searcher program like `grep`.
pub fn searcher() -> &'static str {
    let rg = Path::new(RIPGREP);
    let searcher = if rg.exists() { RIPGREP } else { GREP };
    searcher
}
