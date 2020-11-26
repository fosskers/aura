//! All functionality involving the `-C` command.

use crate::error::Error;
use std::path::Path;

/// Print all package filepaths from the cache that match some search term.
pub fn search(path: &Path, term: String) -> Result<(), Error> {
    let matches = aura_core::cache::search(path, term).map_err(Error::IO)?;
    for file in matches {
        println!("{}", file.display());
    }
    Ok(())
}
