use curl::easy::Easy;
use raur::blocking::Raur;
use raur::{Package, SearchBy};
use serde_derive::Deserialize;
use std::cell::RefCell;
use std::fmt;

#[derive(Deserialize)]
struct Response {
    #[serde(rename = "type")]
    response_type: String,
    error: Option<String>,
    results: Vec<Package>,
}

#[derive(Debug)]
pub enum Error {
    Curl(curl::Error),
    Aur(String),
    Serde(serde_json::Error),
}

impl From<curl::Error> for Error {
    fn from(e: curl::Error) -> Self {
        Error::Curl(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Curl(e) => e.fmt(f),
            Error::Aur(e) => e.fmt(f),
            Error::Serde(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub struct Handle {
    curl: RefCell<Easy>,
    url: String,
}

impl Default for Handle {
    fn default() -> Self {
        Self::new()
    }
}

impl Handle {
    pub fn new() -> Handle {
        Handle {
            curl: RefCell::new(Easy::new()),
            url: raur::AUR_URL.to_string(),
        }
    }

    pub fn new_with_settings<S: Into<String>>(curl: Easy, url: S) -> Handle {
        Handle {
            curl: RefCell::new(curl),
            url: url.into(),
        }
    }

    pub fn url(&self) -> &str {
        &self.url
    }

    fn perform(curl: &mut Easy) -> Result<Vec<Package>, Error> {
        let mut json = Vec::<u8>::new();
        let mut transfer = curl.transfer();
        transfer.write_function(|data| {
            json.extend(data);
            Ok(data.len())
        })?;

        transfer.perform()?;
        drop(transfer);

        let response: Response = serde_json::from_slice(&json)?;

        if response.response_type == "error" {
            Err(Error::Aur(
                response
                    .error
                    .unwrap_or_else(|| "No error message provided".to_string()),
            ))
        } else {
            Ok(response.results)
        }
    }
}

impl Raur for Handle {
    type Err = Error;

    fn raw_info<S: AsRef<str>>(&self, pkg_names: &[S]) -> Result<Vec<Package>, Error> {
        let mut curl = self.curl.borrow_mut();
        let mut url = self.url.clone();
        url.push_str("?v=5&type=info");
        for pkg in pkg_names {
            url.push_str("&arg[]=");
            url.push_str(&curl.url_encode(pkg.as_ref().as_bytes()));
        }
        curl.url(&url)?;
        Self::perform(&mut *curl)
    }

    fn search_by<S: AsRef<str>>(
        &self,
        query: S,
        strategy: SearchBy,
    ) -> Result<Vec<Package>, Error> {
        let mut curl = self.curl.borrow_mut();
        let url = format!(
            "{}?v=5&type=search&by={}&arg={}",
            self.url,
            strategy,
            curl.url_encode(query.as_ref().as_bytes())
        );
        curl.url(&url)?;
        Self::perform(&mut *curl)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search() {
        let handle = Handle::default();

        let query = handle.search("zzzzzzz").unwrap();
        assert_eq!(0, query.len());

        let query = handle.search("spotify").unwrap();
        assert!(!query.is_empty());
        assert!(query[0].url.is_some());
    }

    #[test]
    fn test_info() {
        let handle = Handle::default();

        let query = handle.info(&["pacman-git"]).unwrap();
        assert_eq!(query[0].name, "pacman-git");

        let query = handle.info(&["screens", "screens-git"]);
        assert!(query.is_ok());

        let query = query.unwrap();
        assert_eq!(2, query.len());
    }
}
