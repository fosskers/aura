//! Aura runtime settings.

use crate::dirs;
use crate::error::Nested;
use crate::localization::{identifier_from_code, Localised};
use from_variants::FromVariants;
use i18n_embed_fl::fl;
use log::{debug, error};
use r2d2::Pool;
use r2d2_alpm::{Alpm, AlpmManager};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use unic_langid::LanguageIdentifier;

const DEFAULT_EDITOR: &str = "vi";

#[derive(FromVariants)]
pub(crate) enum Error {
    Dirs(crate::dirs::Error),
    PConf(pacmanconf::Error),
    Alpm(alpm::Error),
    R2d2(r2d2::Error),
    MissingEditor,
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Dirs(e) => e.nested(),
            Error::PConf(e) => error!("{e}"),
            Error::MissingEditor => {}
            Error::Alpm(e) => error!("{e}"),
            Error::R2d2(e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::Dirs(e) => e.localise(fll),
            Error::PConf(_) => fl!(fll, "env-pconf"),
            Error::MissingEditor => fl!(fll, "env-missing-editor"),
            Error::Alpm(_) => fl!(fll, "err-alpm"),
            Error::R2d2(_) => fl!(fll, "err-pool-create"),
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
    fn try_new() -> Option<Self> {
        let config: PathBuf = {
            let c = dirs::aura_config().ok()?;
            if c.is_file() {
                c
            } else {
                dirs::aura_config_old().ok()?
            }
        };

        debug!("Reading: {}", config.display());
        let s = std::fs::read_to_string(config).ok()?;
        toml::from_str(&s).ok()
    }
}

/// Can the `aura.toml` be parsed?
pub(crate) fn parsable_env() -> bool {
    RawEnv::try_new().is_some()
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
        let raw: Option<RawEnv> = RawEnv::try_new();
        let (general, aur, backups) = match raw {
            Some(re) => (
                re.general.map(|rg| rg.into()),
                re.aur.map(|ra| ra.try_into()),
                re.backups.map(|rb| rb.try_into()),
            ),
            None => (None, None, None),
        };

        let e = Env {
            general: general.unwrap_or_default(),
            aur: aur.unwrap_or_else(Aur::try_default)?,
            backups: backups.unwrap_or_else(Backups::try_default)?,
            pacman: pacmanconf::Config::new().map_err(Error::PConf)?,
        };

        Ok(e)
    }

    /// The "sudo string" to be prefixed to certain shell calls.
    pub(crate) fn sudo(&self) -> &'static str {
        if self.general.doas {
            "doas"
        } else {
            "sudo"
        }
    }

    /// Open a series of connections to ALPM handles. The quantity matches the
    /// number of CPUs available on the machine.
    pub(crate) fn alpm_pool(&self) -> Result<Pool<AlpmManager>, Error> {
        // FIXME Thu Jun  9 2022 Unfortunate clone here.
        let mngr = AlpmManager::new(self.pacman.clone());
        let pool = Pool::builder().max_size(self.general.cpus).build(mngr)?;

        Ok(pool)
    }

    /// Open a new connection to `alpm` instance.
    pub(crate) fn alpm(&self) -> Result<Alpm, Error> {
        Alpm::from_config(&self.pacman).map_err(Error::Alpm)
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
    pub(crate) fn reconcile_cli(&mut self, flags: &aura_pm::flags::Args) {
        if let aura_pm::flags::SubCmd::Aur(a) = &flags.subcmd {
            self.aur.reconcile(a)
        }

        // Specifying a language on the command line overrides all other
        // settings.
        if let Some(l) = flags.language() {
            self.general.language = l;
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

#[derive(Debug, Deserialize)]
struct RawGeneral {
    cpus: Option<u32>,
    editor: Option<String>,
    doas: Option<bool>,
    language: Option<String>,
}

#[derive(Debug, Serialize)]
pub(crate) struct General {
    pub(crate) cpus: u32,
    pub(crate) editor: String,
    pub(crate) doas: bool,
    pub(crate) language: LanguageIdentifier,
}

impl Default for General {
    fn default() -> Self {
        Self {
            cpus: num_cpus::get() as u32,
            editor: editor(),
            doas: false,
            language: aura_pm::ENGLISH,
        }
    }
}

impl From<RawGeneral> for General {
    fn from(raw: RawGeneral) -> Self {
        General {
            cpus: raw.cpus.unwrap_or_else(|| num_cpus::get() as u32),
            editor: raw.editor.unwrap_or_else(editor),
            doas: raw.doas.unwrap_or(false),
            // Precedence: We check config first for a language setting. If
            // nothing, we check the environment. If nothing, we fall back to
            // English. This can further be overridden by CLI flags.
            language: raw
                .language
                .and_then(identifier_from_code)
                .or_else(language)
                .unwrap_or(aura_pm::ENGLISH),
        }
    }
}

/// An attempt to fetch a language setting from environment variables.
fn language() -> Option<LanguageIdentifier> {
    let raw = std::env::var("LANG").ok()?;

    // TODO 2024-06-22 Parse out locale so we can avoid countries.
    match raw.as_str() {
        "en_US.UTF-8" => Some(aura_pm::ENGLISH),
        "ja_JP.UTF-8" => Some(aura_pm::JAPANESE),
        "pl_PL.UTF-8" => Some(aura_pm::POLISH),
        "hr_HR.UTF-8" => Some(aura_pm::CROATIAN),
        "sv_SE.UTF-8" => Some(aura_pm::SWEDISH),
        "de_DE.UTF-8" => Some(aura_pm::GERMAN),
        "es_ES.UTF-8" => Some(aura_pm::SPANISH),
        "pt_PT.UTF-8" => Some(aura_pm::PORTUGUESE),
        "fr_FR.UTF-8" => Some(aura_pm::FRENCH),
        "ru_RU.UTF-8" => Some(aura_pm::RUSSIAN),
        "it_IT.UTF-8" => Some(aura_pm::ITALIAN),
        "sr-RS.UTF-8" => Some(aura_pm::SERBIAN),
        "no-NO.UTF-8" => Some(aura_pm::NORWEGIAN),
        "id_ID.UTF-8" => Some(aura_pm::INDONESIAN),
        "zh_CN.UTF-8" => Some(aura_pm::SIMPLIFIED_CHINESE),
        "eo.UTF-8" => Some(aura_pm::ESPERANTO),
        "nl_NL.UTF-8" => Some(aura_pm::DUTCH),
        "tr_TR.UTF-8" => Some(aura_pm::TURKISH),
        "ar_SA.UTF-8" => Some(aura_pm::ARABIC),
        "uk_UA.UTF-8" => Some(aura_pm::UKRAINIAN),
        _ => None,
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
    chroot: HashSet<String>,
    #[serde(default)]
    ignores: HashSet<String>,
    #[serde(default)]
    git: bool,
    #[serde(default)]
    hotedit: bool,
    #[serde(default)]
    diff: bool,
    #[serde(default)]
    delmakedeps: bool,
    #[serde(default)]
    noconfirm: bool,
    #[serde(default)]
    nocheck: bool,
}

#[derive(Debug, Serialize)]
pub(crate) struct Aur {
    pub(crate) build: PathBuf,
    pub(crate) cache: PathBuf,
    pub(crate) clones: PathBuf,
    pub(crate) hashes: PathBuf,
    /// Packages to build via `pkgctl build`.
    pub(crate) chroot: HashSet<String>,
    /// Packages to ignore entirely.
    pub(crate) ignores: HashSet<String>,
    /// Always rebuild VCS packages with `-Au`?
    pub(crate) git: bool,
    /// View/edit PKGBUILDs (etc.) before building.
    pub(crate) hotedit: bool,
    /// View diffs of PKGBUILDs (etc.) before building.
    pub(crate) diff: bool,
    /// Delete makedeps after building.
    pub(crate) delmakedeps: bool,
    /// Don't ask the user for confirmation.
    pub(crate) noconfirm: bool,
    /// Don't consider "checkdeps" during dependency resolution and when calling
    /// `makepkg`.
    pub(crate) nocheck: bool,
}

impl Aur {
    /// Attempt to form sane defaults.
    fn try_default() -> Result<Self, dirs::Error> {
        let a = Aur {
            build: dirs::builds()?,
            cache: dirs::tarballs()?,
            clones: dirs::clones()?,
            hashes: dirs::hashes()?,
            chroot: HashSet::new(),
            ignores: HashSet::new(),
            git: false,
            hotedit: false,
            diff: false,
            delmakedeps: false,
            noconfirm: false,
            nocheck: false,
        };

        Ok(a)
    }

    /// Flags set on the command line should override config settings and other
    /// defaults.
    fn reconcile(&mut self, flags: &aura_pm::flags::Aur) {
        // NOTE 2023-11-26 It may be tempting to save two lines here by blindly
        // setting `self.git` to whatever is found in the `flags` value, but
        // that would cause problems in the case of `true` being set in config,
        // while `false` is used as the default value when no flag were given.
        if flags.git {
            self.git = true;
        }

        if flags.hotedit {
            self.hotedit = true;
        }

        if flags.diff {
            self.diff = true;
        }

        if flags.delmakedeps {
            self.delmakedeps = true;
        }

        if flags.noconfirm {
            self.noconfirm = true;
        }

        if flags.nocheck {
            self.nocheck = true;
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
            chroot: raw.chroot,
            ignores: raw.ignores,
            git: raw.git,
            hotedit: raw.hotedit,
            diff: raw.diff,
            delmakedeps: raw.delmakedeps,
            noconfirm: raw.noconfirm,
            nocheck: raw.nocheck,
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
