//! Viewing and editing configuration files.

use crate::command::misc;
use crate::env::Env;
use crate::flags::Conf;
use crate::utils::ResultVoid;
use from_variants::FromVariants;
use std::env;
use std::process::Command;

#[derive(FromVariants)]
pub(crate) enum Error {
    Env(std::env::VarError),
    Io(std::io::Error),
    Toml(toml::ser::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Env(e) => write!(f, "{e}"),
            Error::Io(e) => write!(f, "{e}"),
            Error::Toml(e) => write!(f, "{e}"),
        }
    }
}

/// The raw contents of a runtime `Env`.
pub(crate) fn general(env: &Env) {
    println!("{:#?}", env);
}

/// Output your current, full Aura config as legal TOML.
pub(crate) fn gen(env: &Env) -> Result<(), Error> {
    let s = toml::ser::to_string_pretty(env)?;
    println!("{s}");
    Ok(())
}

/// Open the `aura.toml` in `bat` or `less`.
pub(crate) fn aura_conf() -> Result<(), Error> {
    let path = crate::dirs::aura_config()?;
    let prog = misc::viewer();
    Command::new(prog).arg(path).status().void()
}

/// Open the `pacman.conf` in `bat` or `less`.
pub(crate) fn pacman_conf(c: Conf) -> Result<(), std::io::Error> {
    let conf = c
        .config
        .unwrap_or_else(|| aura_arch::DEFAULT_PAC_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog).arg(conf).status().void()
}

/// Open the `makepkg.conf` in `bat` or `less`.
pub(crate) fn makepkg_conf() -> Result<(), std::io::Error> {
    let conf =
        env::var("MAKEPKG_CONF").unwrap_or_else(|_| aura_arch::DEFAULT_MAKEPKG_CONF.to_string());
    let prog = misc::viewer();
    Command::new(prog).arg(conf).status().void()
}
