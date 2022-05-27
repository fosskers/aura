//! Aura runtime settings.

use crate::dirs;
use serde::Deserialize;
use std::{collections::HashSet, path::PathBuf};

#[derive(Deserialize)]
struct RawEnv {
    general: Option<RawGeneral>,
    aur: Option<RawAur>,
}

/// Aura's runtime environment, as a combination of settings specified in its
/// config file, as well as options passed from the command line.
pub(crate) struct Env {
    /// Settings applicable to no particular subfeature.
    pub(crate) general: General,
    /// Specifics to the building of AUR packages.
    pub(crate) aur: Aur,
}

impl TryFrom<RawEnv> for Env {
    type Error = dirs::Error;

    fn try_from(raw: RawEnv) -> Result<Self, Self::Error> {
        let e = Env {
            general: raw
                .general
                .map(|rg| rg.try_into())
                .unwrap_or_else(|| General::try_default())?,
            aur: raw
                .aur
                .map(|ra| ra.try_into())
                .unwrap_or_else(|| Aur::try_default())?,
        };

        Ok(e)
    }
}

#[derive(Deserialize)]
struct RawGeneral {
    snapshots: Option<PathBuf>,
}

pub(crate) struct General {
    pub(crate) snapshots: PathBuf,
}

impl General {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let g = General {
            snapshots: dirs::snapshot()?,
        };
        Ok(g)
    }
}

impl TryFrom<RawGeneral> for General {
    type Error = dirs::Error;

    fn try_from(raw: RawGeneral) -> Result<Self, Self::Error> {
        let snapshots = raw.snapshots.map(Ok).unwrap_or_else(|| dirs::snapshot())?;
        let g = General { snapshots };

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
}

pub(crate) struct Aur {
    pub(crate) build: PathBuf,
    pub(crate) cache: PathBuf,
    pub(crate) clones: PathBuf,
    pub(crate) ignores: HashSet<String>,
}

impl Aur {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let a = Aur {
            build: dirs::builds()?,
            cache: dirs::tarballs()?,
            clones: dirs::clones()?,
            ignores: HashSet::new(),
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
        };

        Ok(a)
    }
}
