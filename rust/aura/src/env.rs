//! Aura runtime settings.

use crate::dirs;
use alpm::Alpm;
use from_variants::FromVariants;
use r2d2::Pool;
use r2d2_alpm::AlpmManager;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

const DEFAULT_EDITOR: &str = "vi";

#[derive(FromVariants)]
pub(crate) enum Error {
    Dirs(crate::dirs::Error),
    PConf(pacmanconf::Error),
    Env(std::env::VarError),
    Io(std::io::Error),
    Toml(toml::de::Error),
    MissingEditor,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Dirs(e) => write!(f, "{e}"),
            Error::PConf(e) => write!(f, "{e}"),
            Error::Env(e) => write!(f, "{e}"),
            Error::Io(e) => write!(f, "{e}"),
            Error::Toml(e) => write!(f, "{e}"),
            Error::MissingEditor => write!(f, "Provided EDITOR is not on the PATH!"),
        }
    }
}

#[derive(Deserialize)]
struct RawEnv {
    general: Option<RawGeneral>,
    aur: Option<RawAur>,
    backups: Option<RawBackups>,
}

impl RawEnv {
    /// Attempt to read and parse settings from the filesystem.
    fn try_new() -> Result<Self, Error> {
        let config: PathBuf = dirs::aura_config()?;
        let s = std::fs::read_to_string(config)?;
        let e = toml::from_str(&s)?;
        Ok(e)
    }
}

/// Can the `aura.toml` be parsed?
pub(crate) fn parsable_env() -> bool {
    RawEnv::try_new().is_ok()
}

/// Aura's runtime environment, as a combination of settings specified in its
/// config file, as well as options passed from the command line.
#[derive(Debug, Serialize)]
pub(crate) struct Env {
    /// Settings applicable to no particular subfeature.
    #[allow(dead_code)]
    pub(crate) general: General,
    /// Specifics to the building of AUR packages.
    pub(crate) aur: Aur,
    /// Saving and restoring package states.
    pub(crate) backups: Backups,
    /// Settings from a `pacman.conf`.
    #[serde(skip_serializing)]
    pub(crate) pacman: pacmanconf::Config,
}

impl Env {
    pub(crate) fn try_new() -> Result<Self, Error> {
        // Read the config file, if it's there. We don't actually mind if it isn't,
        // because sensible defaults can (probably) be set anyway.
        let raw: Option<RawEnv> = RawEnv::try_new().ok();
        let (general, aur, backups) = match raw {
            Some(re) => (
                re.general.map(|rg| rg.into()),
                re.aur.map(|ra| ra.try_into()),
                re.backups.map(|rb| rb.try_into()),
            ),
            None => (None, None, None),
        };

        let e = Env {
            general: general.unwrap_or_else(General::default),
            aur: aur.unwrap_or_else(Aur::try_default)?,
            backups: backups.unwrap_or_else(Backups::try_default)?,
            pacman: pacmanconf::Config::new()?,
        };

        Ok(e)
    }

    /// Open a series of connections to ALPM handles. The quantity matches the
    /// number of CPUs available on the machine.
    pub(crate) fn alpm_pool(&self) -> Result<Pool<AlpmManager>, r2d2::Error> {
        // FIXME Thu Jun  9 13:53:49 2022
        //
        // Unfortunate clone here.
        let mngr = AlpmManager::new(self.pacman.clone());
        let pool = Pool::builder().max_size(self.general.cpus).build(mngr)?;

        Ok(pool)
    }

    /// Open a new connection to `alpm` instance.
    pub(crate) fn alpm(&self) -> Result<Alpm, alpm::Error> {
        alpm_utils::alpm_with_conf(&self.pacman)
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

    /// Allow CLI flags to override settings from `aura.toml`.
    pub(crate) fn reconcile_cli(&mut self, flags: &crate::flags::SubCmd) {
        match flags {
            crate::flags::SubCmd::Aur(a) => self.aur.reconcile(a),
            _ => {}
        }
    }

    /// Before continuing, confirm that the settled `Env` is valid to use.
    pub(crate) fn validate(&self) -> Result<(), Error> {
        if self.aur.hotedit {
            which::which(&self.general.editor).map_err(|_| Error::MissingEditor)?;
        }

        Ok(())
    }
}

#[derive(Deserialize)]
struct RawGeneral {
    cpus: Option<u32>,
    editor: Option<String>,
}

#[derive(Debug, Serialize)]
pub(crate) struct General {
    pub(crate) cpus: u32,
    pub(crate) editor: String,
}

impl Default for General {
    fn default() -> Self {
        Self {
            cpus: num_cpus::get() as u32,
            editor: editor(),
        }
    }
}

impl From<RawGeneral> for General {
    fn from(raw: RawGeneral) -> Self {
        General {
            cpus: raw.cpus.unwrap_or_else(|| num_cpus::get() as u32),
            editor: raw.editor.unwrap_or_else(|| editor()),
        }
    }
}

/// The editor program to call in certain situations.
fn editor() -> String {
    std::env::var("EDITOR").unwrap_or_else(|_| DEFAULT_EDITOR.to_string())
}

#[derive(Deserialize)]
struct RawAur {
    build: Option<PathBuf>,
    cache: Option<PathBuf>,
    clones: Option<PathBuf>,
    hashes: Option<PathBuf>,
    #[serde(default)]
    ignores: HashSet<String>,
    #[serde(default)]
    git: bool,
    #[serde(default)]
    hotedit: bool,
    #[serde(default)]
    diff: bool,
}

#[derive(Debug, Serialize)]
pub(crate) struct Aur {
    pub(crate) build: PathBuf,
    pub(crate) cache: PathBuf,
    pub(crate) clones: PathBuf,
    pub(crate) hashes: PathBuf,
    pub(crate) ignores: HashSet<String>,
    /// Always rebuild VCS packages with `-Au`?
    pub(crate) git: bool,
    /// View/edit PKGBUILDs (etc.) before building.
    pub(crate) hotedit: bool,
    /// View diffs of PKGBUILDs (etc.) before building.
    pub(crate) diff: bool,
}

impl Aur {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let a = Aur {
            build: dirs::builds()?,
            cache: dirs::tarballs()?,
            clones: dirs::clones()?,
            hashes: dirs::hashes()?,
            ignores: HashSet::new(),
            git: false,
            hotedit: false,
            diff: false,
        };

        Ok(a)
    }

    /// Flags set on the command line should override config settings and other
    /// defaults.
    fn reconcile(&mut self, flags: &crate::flags::Aur) {
        if flags.git {
            self.git = true;
        }

        if flags.hotedit {
            self.hotedit = true;
        }

        if flags.diff {
            self.diff = true;
        }

        // Harmless clone, as we don't expect many "ignores" to be passed on the
        // command line.
        self.ignores.extend(flags.ignore.clone());
    }
}

impl TryFrom<RawAur> for Aur {
    type Error = dirs::Error;

    fn try_from(raw: RawAur) -> Result<Self, Self::Error> {
        let build = raw.build.map(Ok).unwrap_or_else(dirs::builds)?;
        let cache = raw.cache.map(Ok).unwrap_or_else(dirs::tarballs)?;
        let clones = raw.clones.map(Ok).unwrap_or_else(dirs::clones)?;
        let hashes = raw.hashes.map(Ok).unwrap_or_else(dirs::hashes)?;

        let a = Aur {
            build,
            cache,
            clones,
            hashes,
            ignores: raw.ignores,
            git: raw.git,
            hotedit: raw.hotedit,
            diff: raw.diff,
        };

        Ok(a)
    }
}

#[derive(Deserialize)]
struct RawBackups {
    snapshots: Option<PathBuf>,
}

#[derive(Debug, Serialize)]
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
        let snapshots = raw.snapshots.map(Ok).unwrap_or_else(dirs::snapshot)?;
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
