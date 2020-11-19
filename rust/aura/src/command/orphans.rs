use crate::error::Error;
use alpm::{Alpm, PackageReason, TransFlag};
use aura_arch as arch;
use rustyline::Editor;
use std::collections::HashSet;
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use ubyte::ToByteUnit;

/// Print the name of each orphaned package.
pub fn list(alpm: &Alpm) {
    arch::orphans(&alpm)
        .iter()
        .for_each(|o| println!("{}", o.name()))
}

/// Sets a package's install reason to "as explicit". An alias for `-D --asexplicit`.
pub fn adopt(alpm: &Alpm, packages: Vec<String>) -> Result<(), Error> {
    let db = alpm.localdb();
    let reals: Vec<_> = packages
        .into_iter()
        .filter_map(|p| db.pkg(p).ok())
        .collect();

    if reals.is_empty() {
        Err(Error::NoneExist)
    } else {
        for mut p in reals {
            p.set_reason(PackageReason::Explicit).map_err(Error::Alpm)?;
            agreenln(&format!("{} now marked as explicitly installed.", p.name()))?;
        }

        Ok(())
    }
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
        // Copy the name of each original orphan.
        let names: HashSet<_> = orphans.iter().map(|p| p.name().to_string()).collect();

        // Initialize the transaction.
        let mut flag = TransFlag::RECURSE;
        flag.insert(TransFlag::UNNEEDED);
        alpm.trans_init(flag).map_err(Error::Alpm)?;

        for p in orphans {
            alpm.trans_remove_pkg(p).map_err(Error::Alpm)?;
        }

        // Advance the transaction, calculating the effects of the TransFlags.
        alpm.trans_prepare().map_err(|(_, e)| Error::Alpm(e))?;

        // Notify the user of the results.
        let removal = alpm.trans_remove();
        let longest = removal.iter().map(|p| p.name().len()).max().unwrap_or(0);
        ayellowln("The following orphans and their dependencies will be removed:\n")?;
        for p in removal {
            let size = format!("{}", p.isize().bytes());
            if names.contains(p.name()) {
                cyan(&format!("  {:w$} ", p.name(), w = longest))?;
                println!("{:>9}", size);
            } else {
                println!("  {:w$} {:>9}", p.name(), size, w = longest);
            }
        }
        magenta(&format!("  {:-<w$}\n", "-", w = longest + 10))?;
        let total: i64 = removal.iter().map(|p| p.isize()).sum();
        let size = format!("{}", total.bytes());
        println!("  {:w$} {:>9}\n", "Total", size, w = longest);

        // Proceed with the removal?
        let mut rl = Editor::<()>::new();
        match rl.readline("aura :: Proceed? [Y/n] ") {
            Ok(line) if line.is_empty() || line == "y" || line == "Y" => {
                alpm.trans_commit().map_err(|(_, e)| Error::Alpm(e))?;
                agreenln("Done.")?;
            }
            Ok(_) => Err(Error::Rejected)?,
            Err(e) => Err(Error::RustyLine(e))?,
        }

        alpm.trans_release().map_err(Error::Alpm)?;
    }

    Ok(())
}

fn agreenln(msg: &str) -> Result<(), Error> {
    aurad(Color::Green, true, msg)
}

fn ayellowln(msg: &str) -> Result<(), Error> {
    aurad(Color::Yellow, true, msg)
}

fn cyan(msg: &str) -> Result<(), Error> {
    coloured(Color::Cyan, msg)
}

fn magenta(msg: &str) -> Result<(), Error> {
    coloured(Color::Magenta, msg)
}

/// Doesn't insert a newline.
fn coloured(colour: Color, msg: &str) -> Result<(), Error> {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(colour)))
        .map_err(Error::IO)?;
    write!(&mut stdout, "{}", msg).map_err(Error::IO)?;
    stdout.reset().map_err(Error::IO)
}

/// Optionally inserts a newline.
fn aurad(colour: Color, nl: bool, msg: &str) -> Result<(), Error> {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    write!(&mut stdout, "aura").map_err(Error::IO)?;
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
        .map_err(Error::IO)?;
    write!(&mut stdout, " :: ").map_err(Error::IO)?;
    stdout
        .set_color(ColorSpec::new().set_fg(Some(colour)))
        .map_err(Error::IO)?;
    if nl {
        writeln!(&mut stdout, "{}", msg).map_err(Error::IO)?;
    } else {
        write!(&mut stdout, "{}", msg).map_err(Error::IO)?;
    }
    stdout.reset().map_err(Error::IO)
}
