//! Viewing and editing configuration files.

use crate::command::misc;
use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::PathStr;
use crate::utils::ResultVoid;
use i18n_embed_fl::fl;
use log::error;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

/// The default filepath of the Pacman configuration.
const DEFAULT_PAC_CONF: &str = "/etc/pacman.conf";

pub(crate) enum Error {
    PathToAuraConfig(crate::dirs::Error),
    SerializeEnv(basic_toml::Error),
    CouldntOpen(PathBuf, std::io::Error),
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
            Error::CouldntOpen(p, _) => fl!(fll, "open-err", url = p.utf8()),
        }
    }
}

/// The raw contents of a runtime `Env`.
pub(crate) fn general(env: &Env) {
    println!("{env:#?}");
}

/// Output your current, full Aura config as legal TOML.
pub(crate) fn gen(env: &Env) -> Result<(), Error> {
    let s = basic_toml::to_string(env).map_err(Error::SerializeEnv)?;
    println!("{s}");
    Ok(())
}

/// Open the `$XDG_HOME/aura/config.toml` in `bat` or `less`.
pub(crate) fn open_aura_conf() -> Result<(), Error> {
    let path = crate::dirs::aura_config().map_err(Error::PathToAuraConfig)?;
    let prog = misc::viewer();

    Command::new(prog)
        .arg(&path)
        .status()
        .map_err(|e| Error::CouldntOpen(path, e))
        .void()
}

/// Open the `pacman.conf` in `bat` or `less`.
pub(crate) fn open_pacman_conf() -> Result<(), Error> {
    let conf = Path::new(DEFAULT_PAC_CONF);
    let prog = misc::viewer();

    Command::new(prog)
        .arg(conf)
        .status()
        .map_err(|e| Error::CouldntOpen(conf.to_path_buf(), e))
        .void()
}

/// Open the `makepkg.conf` in `bat` or `less`.
pub(crate) fn open_makepkg_conf() -> Result<(), Error> {
    let path = crate::makepkg::conf_location();
    let prog = misc::viewer();

    Command::new(prog)
        .arg(&path)
        .status()
        .map_err(|e| Error::CouldntOpen(path, e))
        .void()
}
