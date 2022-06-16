//! All functionality involving the `-O` command.

use crate::{green, localization::Localised, proceed, yellow};
use alpm::{Alpm, PackageReason, TransFlag};
use aura_arch as arch;
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::error;
use std::collections::HashSet;
use ubyte::ToByteUnit;

#[derive(FromVariants)]
pub(crate) enum Error {
    #[from_variants(skip)]
    SetExplicit(String, alpm::Error),
    Readline(rustyline::error::ReadlineError),
    Sudo(crate::utils::SudoError),
    #[from_variants(skip)]
    AlpmTx(alpm::Error),
    Cancelled,
    NoneExist,
}

impl Error {
    pub(crate) fn nested(&self) {
        match self {
            Error::SetExplicit(_, e) => error!("{e}"),
            Error::Readline(e) => error!("{e}"),
            Error::Sudo(e) => e.nested(),
            Error::AlpmTx(e) => error!("{e}"),
            Error::Cancelled => {}
            Error::NoneExist => {}
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Readline(_) => fl!(fll, "err-user-input"),
            Error::Sudo(e) => e.localise(fll),
            Error::Cancelled => fl!(fll, "common-cancelled"),
            Error::NoneExist => fl!(fll, "err-none-exist"),
            Error::SetExplicit(p, _) => fl!(fll, "O-explicit-err", pkg = p.as_str()),
            Error::AlpmTx(_) => fl!(fll, "alpm-tx"),
        }
    }
}

/// Print the name of each orphaned package.
pub(crate) fn list(alpm: &Alpm) {
    arch::orphans(alpm).for_each(|o| println!("{}", o.name()))
}

/// Sets a package's install reason to "as explicit". An alias for `-D --asexplicit`.
pub(crate) fn adopt(
    alpm: &Alpm,
    fll: &FluentLanguageLoader,
    packages: Vec<String>,
) -> Result<(), Error> {
    crate::utils::sudo()?;

    let db = alpm.localdb();
    let reals: Vec<_> = packages
        .into_iter()
        .filter_map(|p| db.pkg(p).ok())
        .collect();

    // Exit early if no real packages were given.
    if reals.is_empty() {
        return Err(Error::NoneExist);
    }

    for mut p in reals {
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
pub(crate) fn remove(alpm: &mut Alpm, fll: &FluentLanguageLoader) -> Result<(), Error> {
    crate::utils::sudo()?;

    // Check for orphans.
    let orphans: Vec<_> = arch::orphans(alpm).collect();
    if !orphans.is_empty() {
        // Copy the name of each original orphan.
        let names: HashSet<_> = orphans.iter().map(|p| p.name().to_string()).collect();

        // Initialize the transaction.
        let mut flag = TransFlag::RECURSE;
        flag.insert(TransFlag::UNNEEDED);
        alpm.trans_init(flag).map_err(Error::AlpmTx)?;

        for p in orphans {
            alpm.trans_remove_pkg(p).map_err(Error::AlpmTx)?;
        }

        // Advance the transaction, calculating the effects of the TransFlags.
        alpm.trans_prepare().map_err(|(_, e)| Error::AlpmTx(e))?;

        // Notify the user of the results.
        let removal = alpm.trans_remove();
        let longest = removal.iter().map(|p| p.name().len()).max().unwrap_or(0);
        yellow!(fll, "O-abandon");
        println!();
        for p in removal {
            let size = format!("{}", p.isize().bytes());
            if names.contains(p.name()) {
                print!("  {:w$} ", p.name().cyan(), w = longest);
                println!("{:>9}", size);
            } else {
                println!("  {:w$} {:>9}", p.name(), size, w = longest);
            }
        }
        println!("  {:-<w$}", "-".magenta(), w = longest + 10);
        let total: i64 = removal.iter().map(|p| p.isize()).sum();
        let size = format!("{}", total.bytes());
        println!("  {:w$} {:>9}\n", "Total", size, w = longest);

        // Proceed with the removal if the user accepts.
        proceed!(fll, "proceed").ok_or(Error::Cancelled)?;

        alpm.trans_commit().map_err(|(_, e)| Error::AlpmTx(e))?;
        alpm.trans_release().map_err(Error::AlpmTx)?;
        green!(fll, "common-done");
    }

    Ok(())
}
