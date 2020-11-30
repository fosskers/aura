//! Viewing and editing configuration files.

use crate::command::misc;
use crate::flags::Conf;
use anyhow::{Context, Result};
use std::env;
use std::process::Command;

/// Open the `pacman.conf` in `bat` or `less`.
pub fn pacman_conf(c: Conf) -> Result<()> {
    let conf = c
        .config
        .unwrap_or_else(|| aura_arch::DEFAULT_PAC_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog)
        .arg(conf)
        .status()
        .with_context(|| format!("failed to exec '{}'", prog))?;
    Ok(())
}

/// Open the `makepkg.conf` in `bat` or `less`.
pub fn makepkg_conf() -> Result<()> {
    let conf =
        env::var("MAKEPKG_CONF").unwrap_or_else(|_| aura_arch::DEFAULT_MAKEPKG_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog)
        .arg(conf)
        .status()
        .with_context(|| format!("failed to exec '{}'", prog))?;
    Ok(())
}
