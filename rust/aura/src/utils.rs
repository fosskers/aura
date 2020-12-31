//! Various utility functions.

use crate::error::Error;
use rustyline::Editor;
use std::ffi::OsStr;
use std::process::Command;

// TODO Localize the acceptance chars.
/// Prompt the user for confirmation.
pub(crate) fn prompt(msg: &str) -> Result<(), Error> {
    let mut rl = Editor::<()>::new();
    match rl.readline(msg) {
        Ok(line) if line.is_empty() || line == "y" || line == "Y" => Ok(()),
        Ok(_) => Err(Error::Rejected),
        Err(e) => Err(Error::RustyLine(e)),
    }
}

/// Make a shell call to `pacman`.
pub(crate) fn pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    match Command::new("pacman").args(args).status() {
        Err(e) => Err(Error::IO(e)),
        Ok(es) if es.success() => Ok(()),
        Ok(_) => Err(Error::PacmanError),
    }
}
