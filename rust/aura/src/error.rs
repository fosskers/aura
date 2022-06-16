//! All errors that can occur in the Aura executable.

use crate::localization::Localised;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;

/// Error type for all issues that can occur in the Aura library or executable.
#[derive(FromVariants)]
pub(crate) enum Error {
    A(crate::command::aur::Error),
    B(crate::command::snapshot::Error),
    C(crate::command::cache::Error),
    L(crate::log::Error),
    O(crate::command::orphans::Error),
    Dirs(crate::dirs::Error),
    /// A non-zero exit code was returned from a call to Pacman.
    Pacman(crate::pacman::Error),
    Env(crate::env::Error),
    Conf(crate::conf::Error),
    Check(crate::check::Error),
    Open(crate::open::Error),
    Stats(crate::stats::Error),
}

impl Error {
    /// Log nested errors.
    pub(crate) fn nested(&self) {
        match self {
            Error::A(_) => todo!(),
            Error::B(_) => todo!(),
            Error::C(_) => todo!(),
            Error::L(_) => todo!(),
            Error::O(_) => todo!(),
            Error::Dirs(e) => e.nested(),
            Error::Pacman(e) => e.nested(),
            Error::Env(e) => e.nested(),
            Error::Conf(e) => e.nested(),
            Error::Check(e) => e.nested(),
            Error::Open(e) => e.nested(),
            Error::Stats(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::A(_) => todo!(),
            Error::B(_) => todo!(),
            Error::C(_) => todo!(),
            Error::L(_) => todo!(),
            Error::O(_) => todo!(),
            Error::Dirs(e) => e.localise(fll),
            Error::Pacman(e) => e.localise(fll),
            Error::Env(e) => e.localise(fll),
            Error::Conf(e) => e.localise(fll),
            Error::Check(e) => e.localise(fll),
            Error::Open(e) => e.localise(fll),
            Error::Stats(e) => e.localise(fll),
        }
    }
}
