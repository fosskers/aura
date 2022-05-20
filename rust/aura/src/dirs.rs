//! Directories critical to Aura's function.

use from_variants::FromVariants;
use std::ops::Not;
use std::path::PathBuf;

#[derive(Debug, FromVariants)]
pub enum Error {
    Io(std::io::Error),
    Env(std::env::VarError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{}", e),
            Error::Env(e) => write!(f, "{}", e),
        }
    }
}

/// Fetch the path value of `$XDG_CACHE_HOME` or provide its default according
/// to the specification:
///
/// > `$XDG_CACHE_HOME` defines the base directory relative to which user specific
/// > non-essential data files should be stored. If `$XDG_CACHE_HOME` is either not
/// > set or empty, a default equal to `$HOME/.cache` should be used.
fn xdg_cache() -> Result<PathBuf, std::env::VarError> {
    std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|_| std::env::var("HOME").map(|h| [&h, ".cache"].iter().collect()))
}

/// The full path to the Aura cache.
fn aura_xdg_cache() -> Result<PathBuf, std::env::VarError> {
    let mut cache = xdg_cache()?;
    cache.push("aura");
    Ok(cache)
}

/// The full path to the package snapshot directory.
///
/// Creates the directory if it doesn't exist.
pub(crate) fn snapshot() -> Result<PathBuf, Error> {
    let mut path = aura_xdg_cache()?;
    path.push("snapshots");

    if path.is_dir().not() {
        std::fs::create_dir_all(&path)?;
    }

    Ok(path)
}

/// The full path to the directory of AUR package `git` clones.
///
/// Creates the directory if it doesn't exist.
pub(crate) fn clones() -> Result<PathBuf, Error> {
    let path = std::env::var("AURDEST").map(PathBuf::from).or_else(|_| {
        aura_xdg_cache().map(|mut p| {
            p.push("packages");
            p
        })
    })?;

    if path.is_dir().not() {
        std::fs::create_dir_all(&path)?;
    }

    Ok(path)
}

/// The full path to the build directory.
///
/// Creates the directory if it doesn't exist.
pub(crate) fn builds() -> Result<PathBuf, Error> {
    let mut path = aura_xdg_cache()?;
    path.push("builds");

    if path.is_dir().not() {
        std::fs::create_dir_all(&path)?;
    }

    Ok(path)
}

/// The full path to the Aura-specific tarball cache.
///
/// Creates the directory if it doesn't exist.
pub(crate) fn tarballs() -> Result<PathBuf, Error> {
    let mut path = aura_xdg_cache()?;
    path.push("cache");

    if path.is_dir().not() {
        std::fs::create_dir_all(&path)?;
    }

    Ok(path)
}
