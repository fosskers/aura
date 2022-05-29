//! Aura runtime settings.

use crate::dirs;
use from_variants::FromVariants;
use serde::Deserialize;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

#[derive(FromVariants)]
pub(crate) enum Error {
    Dirs(crate::dirs::Error),
    PConf(pacmanconf::Error),
    Env(std::env::VarError),
    Io(std::io::Error),
    Toml(toml::de::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Dirs(e) => write!(f, "{e}"),
            Error::PConf(e) => write!(f, "{e}"),
            Error::Env(e) => write!(f, "{e}"),
            Error::Io(e) => write!(f, "{e}"),
            Error::Toml(e) => write!(f, "{e}"),
        }
    }
}

#[derive(Deserialize)]
struct RawEnv {
    general: Option<RawGeneral>,
    aur: Option<RawAur>,
    backups: Option<RawBackups>,
}

/// Aura's runtime environment, as a combination of settings specified in its
/// config file, as well as options passed from the command line.
#[derive(Debug)]
pub(crate) struct Env {
    /// Settings applicable to no particular subfeature.
    pub(crate) general: General,
    /// Specifics to the building of AUR packages.
    pub(crate) aur: Aur,
    /// Saving and restoring package states.
    pub(crate) backups: Backups,
    /// Settings from a `pacman.conf`.
    pub(crate) pacman: pacmanconf::Config,
}

impl Env {
    pub(crate) fn try_new() -> Result<Self, Error> {
        // Read the config file, if it's there. We don't actually mind if it isn't,
        // because sensible defaults can (probably) be set anyway.
        let config = dirs::aura_config()?;
        let raw: Option<RawEnv> = std::fs::read_to_string(config)
            .ok()
            .and_then(|s| toml::from_str(&s).ok());
        let (general, aur, backups) = match raw {
            Some(re) => (
                re.general.map(|rg| rg.try_into()),
                re.aur.map(|ra| ra.try_into()),
                re.backups.map(|rb| rb.try_into()),
            ),
            None => (None, None, None),
        };

        let e = Env {
            general: general.unwrap_or_else(|| General::try_default())?,
            aur: aur.unwrap_or_else(|| Aur::try_default())?,
            backups: backups.unwrap_or_else(|| Backups::try_default())?,
            pacman: pacmanconf::Config::new()?,
        };

        Ok(e)
    }

    /// All tarball caches across the various config sources.
    pub(crate) fn caches(&self) -> Vec<&Path> {
        self.pacman
            .cache_dir
            .iter()
            .map(Path::new)
            .chain(std::iter::once(self.aur.cache.as_ref()))
            .collect()
    }

    /// Path to the ALPM log file.
    pub(crate) fn alpm_log(&self) -> &Path {
        Path::new(&self.pacman.log_file)
    }
}

#[derive(Deserialize)]
struct RawGeneral {}

#[derive(Debug)]
pub(crate) struct General {}

impl General {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let g = General {};
        Ok(g)
    }
}

impl TryFrom<RawGeneral> for General {
    type Error = dirs::Error;

    fn try_from(_: RawGeneral) -> Result<Self, Self::Error> {
        let g = General {};

        Ok(g)
    }
}

#[derive(Deserialize)]
struct RawAur {
    build: Option<PathBuf>,
    cache: Option<PathBuf>,
    clones: Option<PathBuf>,
    #[serde(default)]
    ignores: HashSet<String>,
    git: bool,
}

#[derive(Debug)]
pub(crate) struct Aur {
    pub(crate) build: PathBuf,
    pub(crate) cache: PathBuf,
    pub(crate) clones: PathBuf,
    pub(crate) ignores: HashSet<String>,
    /// Always rebuild VCS packages with `-Au`?
    pub(crate) git: bool,
}

impl Aur {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let a = Aur {
            build: dirs::builds()?,
            cache: dirs::tarballs()?,
            clones: dirs::clones()?,
            ignores: HashSet::new(),
            git: false,
        };

        Ok(a)
    }
}

impl TryFrom<RawAur> for Aur {
    type Error = dirs::Error;

    fn try_from(raw: RawAur) -> Result<Self, Self::Error> {
        let build = raw.build.map(Ok).unwrap_or_else(|| dirs::builds())?;
        let cache = raw.cache.map(Ok).unwrap_or_else(|| dirs::tarballs())?;
        let clones = raw.clones.map(Ok).unwrap_or_else(|| dirs::clones())?;

        let a = Aur {
            build,
            cache,
            clones,
            ignores: raw.ignores,
            git: raw.git,
        };

        Ok(a)
    }
}

#[derive(Deserialize)]
struct RawBackups {
    snapshots: Option<PathBuf>,
}

#[derive(Debug)]
pub(crate) struct Backups {
    pub(crate) snapshots: PathBuf,
}

impl Backups {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let g = Backups {
            snapshots: dirs::snapshot()?,
        };
        Ok(g)
    }
}

impl TryFrom<RawBackups> for Backups {
    type Error = dirs::Error;

    fn try_from(raw: RawBackups) -> Result<Self, Self::Error> {
        let snapshots = raw.snapshots.map(Ok).unwrap_or_else(|| dirs::snapshot())?;
        let g = Backups { snapshots };

        Ok(g)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_config_file() {
        let e = toml::from_str::<RawEnv>("").unwrap();
        assert!(e.general.is_none());
        assert!(e.aur.is_none());
        assert!(e.backups.is_none());
    }

    #[test]
    fn bare_config() {
        let file = std::fs::read_to_string("tests/bare-config.toml").unwrap();
        let e = toml::from_str::<RawEnv>(&file).unwrap();
        assert!(e.general.is_some());
        assert!(e.aur.is_some());
        assert!(e.backups.is_some());
    }

    #[test]
    fn simple_config() {
        let file = std::fs::read_to_string("tests/simple-config.toml").unwrap();
        let aur = toml::from_str::<RawEnv>(&file).unwrap().aur.unwrap();
        let exp: HashSet<_> = ["foo".to_string(), "bar".to_string()].into();
        assert_eq!(exp, aur.ignores);
    }
}
