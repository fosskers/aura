//! Viewing and editing configuration files.

use crate::command::misc;
use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::{PathStr, ResultVoid};
use aura::flags::Conf;
use from_variants::FromVariants;
use i18n_embed_fl::fl;
use log::error;
use std::env;
use std::process::Command;

/// The default filepath of the Pacman configuration.
const DEFAULT_PAC_CONF: &str = "/etc/pacman.conf";

// TODO Handle the other potential default locations.
/// The default filepath of the Makepkg configuration.
const DEFAULT_MAKEPKG_CONF: &str = "/etc/makepkg.conf";

#[derive(FromVariants)]
pub(crate) enum Error {
    PathToAuraConfig(crate::dirs::Error),
    SerializeEnv(toml::ser::Error),
    #[from_variants(skip)]
    CouldntOpen(String, std::io::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::PathToAuraConfig(e) => e.nested(),
            Error::SerializeEnv(e) => error!("{e}"),
            Error::CouldntOpen(_, e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::PathToAuraConfig(_) => fl!(fll, "err-config-path"),
            Error::SerializeEnv(_) => fl!(fll, "conf-toml-err"),
            Error::CouldntOpen(p, _) => fl!(fll, "open-err", url = p.as_str()),
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

    Command::new(prog)
        .arg(&path)
        .status()
        .map_err(|e| Error::CouldntOpen(path.utf8(), e))
        .void()
}

/// Open the `pacman.conf` in `bat` or `less`.
pub(crate) fn pacman_conf(c: Conf) -> Result<(), Error> {
    let conf = c.config.unwrap_or_else(|| DEFAULT_PAC_CONF.to_string());
    let prog = misc::viewer();

    Command::new(prog)
        .arg(&conf)
        .status()
        .map_err(|e| Error::CouldntOpen(conf, e))
        .void()
}

/// Open the `makepkg.conf` in `bat` or `less`.
pub(crate) fn makepkg_conf() -> Result<(), Error> {
    let conf = env::var("MAKEPKG_CONF").unwrap_or_else(|_| DEFAULT_MAKEPKG_CONF.to_string());
    let prog = misc::viewer();

    Command::new(prog)
        .arg(&conf)
        .status()
        .map_err(|e| Error::CouldntOpen(conf, e))
        .void()
}
