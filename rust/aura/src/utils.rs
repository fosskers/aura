//! Various utility functions.

use anyhow::{Context, Result};
use rustyline::Editor;

/// Prompt the user for confirmation.
pub fn prompt(msg: &str) -> Result<bool> {
    let mut rl = Editor::<()>::new();
    let line = rl.readline(msg).context("failed to read line")?;
    Ok(line.is_empty() || line.to_lowercase() == "y")
}
