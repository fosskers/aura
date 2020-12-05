//! Viewing and editing configuration files.

use crate::command::misc;
use crate::error::Error;
use crate::flags::Conf;
use std::env;
use std::process::Command;

/// Open the `pacman.conf` in `bat` or `less`.
pub(crate) fn pacman_conf(c: Conf) -> Result<(), Error> {
    let conf = c
        .config
        .unwrap_or_else(|| aura_arch::DEFAULT_PAC_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog).arg(conf).status()?;
    Ok(())
}

/// Open the `makepkg.conf` in `bat` or `less`.
pub(crate) fn makepkg_conf() -> Result<(), Error> {
    let conf =
        env::var("MAKEPKG_CONF").unwrap_or_else(|_| aura_arch::DEFAULT_MAKEPKG_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog).arg(conf).status()?;
    Ok(())
}
