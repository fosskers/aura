//! All functionality involving the `-O` command.

use crate::env::Env;
use crate::error::Nested;
use crate::green;
use crate::localization::Localised;
use crate::utils::NOTHING;
use applying::Apply;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use r2d2_alpm::Alpm;
use std::ops::Not;

pub(crate) enum Error {
    Removal(crate::pacman::Error),
    Adopt(crate::pacman::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Removal(e) => e.nested(),
            Error::Adopt(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Removal(e) => e.localise(fll),
            Error::Adopt(e) => e.localise(fll),
        }
    }
}

/// Print the name of each orphaned package.
pub(crate) fn list(alpm: &Alpm) {
    aura_core::orphans(alpm).for_each(|o| println!("{} {}", o.name(), o.version()))
}

/// Print the name of each "elderly" package. In theory these are all explicitly
/// installed applications, but occasionally packages are installed by mistake
/// or forgotten. We want to identify such packages for removal.
pub(crate) fn elderly(alpm: &Alpm) {
    aura_core::elderly(alpm).for_each(|o| println!("{} {}", o.name(), o.version()))
}

/// Sets a package's install reason to "as explicit". An alias for `-D --asexplicit`.
pub(crate) fn adopt(env: &Env, packages: Vec<String>) -> Result<(), Error> {
    crate::pacman::sudo_pacman(env, "-D", ["--asexplicit"], packages).map_err(Error::Adopt)
}

/// Uninstall all orphan packages.
///
/// Will fail if the process does not have permission to create the lockfile,
/// which usually lives in a root-owned directory.
pub(crate) fn remove(env: &Env, alpm: &Alpm, fll: &FluentLanguageLoader) -> Result<(), Error> {
    let orphans: Vec<_> = aura_core::orphans(alpm).collect();

    if orphans.is_empty().not() {
        orphans
            .iter()
            .map(|p| p.name())
            .apply(|names| crate::pacman::sudo_pacman(env, "-Rsu", NOTHING, names))
            .map_err(Error::Removal)?;

        green!(fll, "common-done");
    }

    Ok(())
}
