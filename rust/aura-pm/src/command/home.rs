//! Specification of a consistent system environment that you can easily port to
//! other machines. It allows you to specify packages that you always want
//! installed, as well as automatically managed symlinks to various config
//! files.
//!
//! Obviously this is not as fully featured as Guix or Nix, but it does
//! formalize a basic setup that people often use Guix Home or write ad hoc
//! scripts for, and thus it rests somewhere between the two poles.

use crate::dirs::xdg_config;
use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;

pub(crate) enum Error {
    Dirs(crate::dirs::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Dirs(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::Dirs(e) => e.localise(fll),
        }
    }
}

pub(crate) fn apply(env: &Env) -> Result<(), Error> {
    let xdg = xdg_config().map_err(Error::Dirs)?;

    if let Some(home) = env.home.as_ref() {
        for link in home.links.iter() {
            println!("{:?}", link.status(&xdg));
        }
    }

    Ok(())
}
