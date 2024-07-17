//! Fetching data from remote endpoints.

use crate::error::Nested;
use crate::localization::Localised;
use curl::easy::Easy;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::debug;
use log::error;
use serde::de::DeserializeOwned;

pub enum Error {
    Curl(curl::Error),
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
            Error::Curl(e) => fl!(fll, "err-curl", err = e.to_string()),
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
    handle.url(url).map_err(Error::Curl)?;
    handle.fail_on_error(true).map_err(Error::Curl)?;

    // Blocked off to allow `data` to be borrowed again down below.
    {
        let mut tx = handle.transfer();
        tx.write_function(|bytes| {
            data.extend_from_slice(bytes);
            Ok(bytes.len())
        })
        .map_err(Error::Curl)?;
        tx.perform().map_err(Error::Curl)?;
    }

    let json = serde_json::from_slice(&data).map_err(|e| Error::Json(url.to_string(), e))?;
    Ok(json)
}
