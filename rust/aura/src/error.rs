//! Errors

use anyhow::Result;
use std::fmt;

/// An error that should not display
#[derive(Debug)]
pub struct Silent;

impl fmt::Display for Silent {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl std::error::Error for Silent {}

/// Creates a silent anyhow error.
pub fn silent() -> Result<()> {
    Err(Silent.into())
}
