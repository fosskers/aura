//! Viewing the Pacman/ALPM log.

use crate::command::misc;
use crate::error::Error;
use std::process::Command;

/// Open the Pacman/ALPM log in `bat` or `less`.
pub fn view_log(path: Option<String>) -> Result<(), Error> {
    let logp = path.unwrap_or_else(|| aura_arch::DEFAULT_LOG.to_string());
    let prog = misc::viewer();
    Command::new(prog).arg(logp).status().map_err(Error::IO)?;
    Ok(())
}
