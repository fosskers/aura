//! Types and utilities for the spawing of an R2D2 [`Pool`] that handles
//! multiple connections to an Arch Linux [`Alpm`] database.

#![warn(missing_docs)]

use alpm::Alpm;
use pacmanconf::Config;
use r2d2::ManageConnection;

/// Manage multiple [`Alpm`] handles.
pub struct AlpmManager {
    /// The result of having parsed a `pacman.conf` file.
    config: Config,
}

impl ManageConnection for AlpmManager {
    type Connection = Alpm;
    type Error = alpm::Error;

    fn connect(&self) -> Result<Self::Connection, Self::Error> {
        alpm_utils::alpm_with_conf(&self.config)
    }

    fn is_valid(&self, _: &mut Self::Connection) -> Result<(), Self::Error> {
        Ok(())
    }

    fn has_broken(&self, _: &mut Self::Connection) -> bool {
        false
    }
}
