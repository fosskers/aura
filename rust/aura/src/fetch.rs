//! Fetching data from remote endpoints.

use curl::easy::Easy;
use from_variants::FromVariants;
use serde::de::DeserializeOwned;

#[derive(FromVariants)]
pub enum Error {
    Curl(curl::Error),
    Json(serde_json::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Curl(e) => write!(f, "{}", e),
            Error::Json(e) => write!(f, "{}", e),
        }
    }
}

/// Fetch JSON from some given endpoint.
pub(crate) fn fetch_json<T>(url: &str) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    let mut handle = Easy::new();
    let mut data = Vec::new();
    handle.url(url)?;
    handle.fail_on_error(true)?;

    // Blocked off to allow `data` to be borrowed again down below.
    {
        let mut tx = handle.transfer();
        tx.write_function(|bytes| {
            data.extend_from_slice(bytes);
            Ok(bytes.len())
        })?;
        tx.perform()?;
    }

    let json = serde_json::from_slice(&data)?;
    Ok(json)
}
