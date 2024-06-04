//! Fetching data from remote endpoints.

use crate::error::Nested;
use crate::localization::Localised;
use curl::easy::Easy;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::{debug, error};
use serde::de::DeserializeOwned;

#[derive(FromVariants)]
pub enum Error {
    Curl(curl::Error),
    #[from_variants(skip)]
    Json(String, serde_json::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Curl(e) => error!("{e}"),
            Error::Json(_, e) => error!("{e}"),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Curl(_) => fl!(fll, "err-curl"),
            Error::Json(url, _) => fl!(fll, "err-json-decode", url = url.as_str()),
        }
    }
}

/// Fetch JSON from some given endpoint.
pub(crate) fn fetch_json<T>(url: &str) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    debug!("CURL calling {url}");

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

    let json = serde_json::from_slice(&data).map_err(|e| Error::Json(url.to_string(), e))?;
    Ok(json)
}
