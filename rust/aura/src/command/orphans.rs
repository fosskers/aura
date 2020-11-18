use alpm::{Alpm, TransFlag};
use aura_arch as arch;
use ubyte::ToByteUnit;

/// Print the name of each orphaned package.
pub fn list(alpm: &Alpm) {
    arch::orphans(&alpm)
        .iter()
        .for_each(|o| println!("{}", o.name()))
}

// NOTES
// `trans_commit` will fail if the NO_LOCK flag is set.

/// Uninstall all orphan packages.
///
/// Will fail if the process does not have permission to create the lockfile,
/// which usually lives in a root-owned directory.
pub fn remove(alpm: &mut Alpm) -> Result<(), alpm::Error> {
    // Check for orphans.
    let orphans = arch::orphans(alpm);
    if !orphans.is_empty() {
        // Initialize the transaction.
        let mut flag = TransFlag::RECURSE;
        flag.insert(TransFlag::UNNEEDED);
        flag.insert(TransFlag::NO_LOCK);
        alpm.trans_init(flag)?;

        for p in orphans {
            alpm.trans_remove_pkg(p)?;
        }

        // TODO Prompt user.

        // Advance the transaction, calculating the effects of the TransFlags.
        alpm.trans_prepare().map_err(|(_, e)| e)?;
        let removal = alpm.trans_remove();
        let longest = removal.iter().map(|p| p.name().len()).max().unwrap_or(0);
        println!("aura :: The following orphans and their dependencies will be removed:\n");
        for p in removal {
            let size = format!("{}", p.isize().bytes());
            println!("  {:w$} {:>9}", p.name(), size, w = longest);
        }

        // Remove the packages.
        // alpm.trans_commit().map_err(|(_, e)| e)?;
        alpm.trans_release()?;
    }

    Ok(())
}
