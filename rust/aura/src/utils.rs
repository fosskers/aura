//! Various utility functions.

use crate::error::Error;
use rustyline::Editor;

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
