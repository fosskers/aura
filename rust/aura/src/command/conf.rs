//! Viewing and editing configuration files.

use crate::command::misc;
use crate::error::Error;
use crate::flags::Conf;
use alpm::Alpm;
use pacmanconf::Config;
use std::env;
use std::process::Command;

/// General settings.
pub(crate) fn general(alpm: &Alpm, config: Config) {
    let syncs = alpm.syncdbs();

    for repo in config.repos {
        println!("{}", repo.name);
        for server in repo.servers {
            println!("  {}", server);
        }
    }
}

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
