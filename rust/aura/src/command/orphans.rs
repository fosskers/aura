use alpm::{Alpm, TransFlag};
use aura_arch as arch;

/// Print the name of each orphaned package.
pub fn list(alpm: &Alpm) {
    arch::orphans(&alpm)
        .iter()
        .for_each(|o| println!("{}", o.name()))
}

// NOTES
// `trans_commit` will fail if the NO_LOCK flag is set.

/// Uninstall all orphan packages.
pub fn remove(alpm: &mut Alpm) -> Result<(), alpm::Error> {
    // Initialize the transaction.
    let mut flag = TransFlag::RECURSE;
    flag.insert(TransFlag::UNNEEDED);
    alpm.trans_init(flag)?;

    // Set the packages for removal.
    for p in arch::orphans(alpm) {
        alpm.trans_remove_pkg(p)?;
    }

    // Remove the packages.
    alpm.trans_prepare().map_err(|(_, e)| e)?;
    alpm.trans_commit().map_err(|(_, e)| e)?;
    alpm.trans_release()
}
