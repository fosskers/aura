//! Viewing and editing configuration files.

use crate::command::misc;
use crate::error::Error;
use crate::flags::Conf;
use std::process::Command;

/// Open the `pacman.conf` in `bat` or `less`.
pub fn pacman_conf(c: Conf) -> Result<(), Error> {
    let conf = c.config.unwrap_or(aura_arch::DEFAULT_PAC_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog).arg(conf).status().map_err(Error::IO)?;
    Ok(())
}

// TODO Check `MAKEPKG_CONF` env var.
/// Open the `makepkg.conf` in `bat` or `less`.
pub fn makepkg_conf() -> Result<(), Error> {
    let conf = aura_arch::DEFAULT_MAKEPKG_CONF;
    let prog = misc::viewer();
    Command::new(prog).arg(conf).status().map_err(Error::IO)?;
    Ok(())
}
