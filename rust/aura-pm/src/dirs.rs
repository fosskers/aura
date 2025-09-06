//! Directories critical to Aura's function.

use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::PathStr;
use i18n_embed_fl::fl;
use log::error;
use std::ops::Not;
use std::path::PathBuf;

#[derive(Debug)]
pub enum Error {
    Mkdir(PathBuf, std::io::Error),
    XdgHome(std::env::VarError),
    XdgCache(std::env::VarError),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Mkdir(_, e) => error!("{e}"),
            Error::XdgHome(e) => error!("{e}"),
            Error::XdgCache(e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::Mkdir(p, _) => fl!(fll, "dir-mkdir", dir = p.utf8()),
            Error::XdgHome(_) => fl!(fll, "dir-home"),
            Error::XdgCache(_) => fl!(fll, "dir-cache"),
        }
    }
}

/// Like [`xdg_cache`], but for `XDG_CONFIG_HOME`.
pub(crate) fn xdg_config() -> Result<PathBuf, Error> {
    std::env::var("XDG_CONFIG_HOME")
        .map(PathBuf::from)
        .or_else(|_| std::env::var("HOME").map(|h| [&h, ".config"].iter().collect()))
        .map_err(Error::XdgHome)
}

/// The location of Aura's config file.
///
/// `~/.config/aura/` is automatically created if it doesn't exist.
pub(crate) fn aura_config() -> Result<PathBuf, Error> {
    let xdg = xdg_config()?;
    let dir = xdg.join("aura");

    ensure_dir(&dir)?;

    Ok(dir.join("config.toml"))
}

/// Fetch the path value of `$XDG_CACHE_HOME` or provide its default according
/// to the specification:
///
/// > `$XDG_CACHE_HOME` defines the base directory relative to which user specific
/// > non-essential data files should be stored. If `$XDG_CACHE_HOME` is either not
/// > set or empty, a default equal to `$HOME/.cache` should be used.
fn xdg_cache() -> Result<PathBuf, Error> {
    std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|_| std::env::var("HOME").map(|h| [&h, ".cache"].iter().collect()))
        .map_err(Error::XdgCache)
}

/// The full path to the Aura cache.
pub(crate) fn aura_xdg_cache() -> Result<PathBuf, Error> {
    Ok(xdg_cache()?.join("aura"))
}

/// The full path to the package snapshot directory.
pub(crate) fn snapshot() -> Result<PathBuf, Error> {
    Ok(aura_xdg_cache()?.join("snapshots"))
}

/// The full path to the directory of AUR package `git` clones.
pub(crate) fn clones() -> Result<PathBuf, Error> {
    let path = std::env::var("AURDEST")
        .map(PathBuf::from)
        .or_else(|_| aura_xdg_cache().map(|p| p.join("packages")))?;

    Ok(path)
}

/// The full path to the build directory.
pub(crate) fn builds() -> Result<PathBuf, Error> {
    Ok(aura_xdg_cache()?.join("builds"))
}

/// The full path to the Aura-specific tarball cache.
pub(crate) fn tarballs() -> Result<PathBuf, Error> {
    Ok(aura_xdg_cache()?.join("cache"))
}

/// The full path to the directory of git hashes that indicate the last time an
/// AUR package was built and installed.
pub(crate) fn hashes() -> Result<PathBuf, Error> {
    Ok(aura_xdg_cache()?.join("hashes"))
}

/// Create a directory if it doesn't exist.
pub(crate) fn ensure_dir(path: &PathBuf) -> Result<(), Error> {
    if path.is_dir().not() {
        std::fs::create_dir_all(&path).map_err(|e| Error::Mkdir(path.clone(), e))?;
    }

    Ok(())
}
