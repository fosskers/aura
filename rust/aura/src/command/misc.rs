//! Miscellaneous functionality.

use crate::error::Error;
use crate::flags::PacConf;
use std::path::Path;
use std::process::Command;

/// Expected location of the `bat` executable if installed from official repos.
const BAT: &str = "/bin/bat";

/// Expected location of the `less` executable.
const LESS: &str = "/bin/less";

/// Open the `pacman.conf` in `bat` or `less`.
pub fn pacman_conf(pc: PacConf) -> Result<(), Error> {
    let conf = pc.config.unwrap_or(aura_arch::DEFAULT_PAC_CONF.to_string());
    let path = Path::new("/bin/bat");
    let prog = if path.exists() { BAT } else { LESS };

    Command::new(prog).arg(conf).status().map_err(Error::IO)?;
    Ok(())
}

/// Display the locales that Aura has been translated to.
pub fn languages() {
    for lang in crate::localization::available_languages() {
        println!("{}", lang);
    }
}
