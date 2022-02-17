//! Types and utilities for the spawing of a [`r2d2::Pool`] that handles
//! multiple connections to an Arch Linux [`Alpm`] database.
//!
//! # Usage
//!
//! To create a `Pool` that delegates [`Alpm`] connections within (for instance)
//! some Rayon threads:
//!
//! ```
//! use r2d2::Pool;
//! use r2d2_alpm::AlpmManager;
//! use rayon::prelude::*;
//!
//! let mngr = AlpmManager::from_file("/etc/pacman.conf").unwrap();
//! let pool = Pool::builder().max_size(4).build(mngr).unwrap();
//!
//! (0..10).into_par_iter().for_each(|n| {
//!     // `Pool::get` will wait for a configurable length
//!     // of time for a free connection before giving up.
//!     if let Ok(alpm) = pool.get() {
//!         // Use the ALPM handle freely here.
//!     }
//! });
//! ```
//!
//! Like [`std::sync::Arc`], `Pool` is cheap to [`Clone`], and can be passed
//! around freely to subthreads.

#![warn(missing_docs)]

use alpm::Alpm;
use pacmanconf::Config;
use r2d2::ManageConnection;
use std::ffi::OsStr;

/// Manage multiple [`Alpm`] handles.
pub struct AlpmManager {
    /// The result of having parsed a `pacman.conf` file.
    config: Config,
}

impl AlpmManager {
    /// Construct a new `AlpmManager` from a [`Config`] you've somehow obtained.
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    /// Try to construct a new `AlpmManager` by reading a `pacman.conf` found at
    /// the given filepath.
    pub fn from_file<P>(path: P) -> Result<Self, pacmanconf::Error>
    where
        P: AsRef<OsStr>,
    {
        let config = Config::from_file(path)?;
        let manager = Self { config };

        Ok(manager)
    }

    /// An immutable borrow of the inner [`Config`].
    pub fn config(&self) -> &Config {
        &self.config
    }
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
