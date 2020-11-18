use crate::error::Error;
use alpm::{Alpm, TransFlag};
use aura_arch as arch;
use rustyline::Editor;
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
pub fn remove(alpm: &mut Alpm) -> Result<(), Error> {
    // Check for orphans.
    let orphans = arch::orphans(alpm);
    if !orphans.is_empty() {
        // Initialize the transaction.
        let mut flag = TransFlag::RECURSE;
        flag.insert(TransFlag::UNNEEDED);
        flag.insert(TransFlag::NO_LOCK);
        alpm.trans_init(flag).map_err(Error::Alpm)?;

        for p in orphans {
            alpm.trans_remove_pkg(p).map_err(Error::Alpm)?;
        }

        // Advance the transaction, calculating the effects of the TransFlags.
        alpm.trans_prepare().map_err(|(_, e)| Error::Alpm(e))?;
        let removal = alpm.trans_remove();
        let longest = removal.iter().map(|p| p.name().len()).max().unwrap_or(0);
        println!("aura :: The following orphans and their dependencies will be removed:\n");
        for p in removal {
            let size = format!("{}", p.isize().bytes());
            println!("  {:w$} {:>9}", p.name(), size, w = longest);
        }
        println!();

        // Proceed with the removal?
        let mut rl = Editor::<()>::new();
        match rl.readline("aura :: Proceed? [Y/n] ") {
            Ok(line) if line.is_empty() || line == "y" || line == "Y" => {
                println!("Go!");
                // alpm.trans_commit().map_err(|(_, e)| e)?;
            }
            _ => {}
        }

        alpm.trans_release().map_err(Error::Alpm)?;
    }

    Ok(())
}
