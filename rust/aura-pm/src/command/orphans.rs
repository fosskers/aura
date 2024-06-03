//! All functionality involving the `-O` command.

use crate::env::Env;
use crate::error::Nested;
use crate::green;
use crate::localization::Localised;
use crate::utils::NOTHING;
use alpm::PackageReason;
use aura_core::Apply;
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use r2d2_alpm::Alpm;
use std::ops::Not;

#[derive(FromVariants)]
pub(crate) enum Error {
    #[from_variants(skip)]
    SetExplicit(String, alpm::Error),
    Readline(rustyline::error::ReadlineError),
    Sudo(crate::utils::SudoError),
    NoneExist,
    Removal(crate::pacman::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::SetExplicit(_, e) => error!("{e}"),
            Error::Readline(e) => error!("{e}"),
            Error::Sudo(e) => e.nested(),
            Error::NoneExist => {}
            Error::Removal(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Readline(_) => fl!(fll, "err-user-input"),
            Error::Sudo(e) => e.localise(fll),
            Error::NoneExist => fl!(fll, "err-none-exist"),
            Error::SetExplicit(p, _) => fl!(fll, "O-explicit-err", pkg = p.as_str()),
            Error::Removal(e) => e.localise(fll),
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
pub(crate) fn adopt(
    env: &Env,
    alpm: &Alpm,
    fll: &FluentLanguageLoader,
    // TODO 2024-03-18 Make this NEVec.
    packages: Vec<String>,
) -> Result<(), Error> {
    crate::utils::sudo(env)?;

    let db = alpm.as_ref().localdb();
    let reals: Vec<_> = packages
        .into_iter()
        .filter_map(|p| db.pkg(p).ok())
        .collect();

    // Exit early if no real packages were given.
    if reals.is_empty() {
        return Err(Error::NoneExist);
    }

    for p in reals {
        p.set_reason(PackageReason::Explicit)
            .map_err(|e| Error::SetExplicit(p.name().to_string(), e))?;
        green!(fll, "O-adopt", pkg = p.name());
    }

    Ok(())
}

/// Uninstall all orphan packages.
///
/// Will fail if the process does not have permission to create the lockfile,
/// which usually lives in a root-owned directory.
pub(crate) fn remove(alpm: &Alpm, fll: &FluentLanguageLoader) -> Result<(), Error> {
    let orphans: Vec<_> = aura_core::orphans(alpm).collect();

    if orphans.is_empty().not() {
        orphans
            .iter()
            .map(|p| p.name())
            .apply(|names| crate::pacman::sudo_pacman("-Rsu", NOTHING, names))
            .map_err(Error::Removal)?;

        green!(fll, "common-done");
    }

    Ok(())
}
