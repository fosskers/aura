//! All errors that can occur in the Aura executable.

use crate::localization::Localised;
use from_variants::FromVariants;

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
    /// An error occurred within the `alpm` C code.
    Alpm(alpm::Error),
    /// An error during logger initialization.
    TerminalLogger(log::SetLoggerError),
    /// An error parsing `pacman.conf`.
    PacConf(pacmanconf::Error),
    Env(crate::env::Error),
    Conf(crate::conf::Error),
    Check(crate::check::Error),
    Open(crate::open::Error),
    Stats(crate::stats::Error),
}

impl Localised for Error {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        match self {
            Error::A(_) => todo!(),
            Error::B(_) => todo!(),
            Error::C(_) => todo!(),
            Error::L(_) => todo!(),
            Error::O(_) => todo!(),
            Error::Dirs(_) => todo!(),
            Error::Pacman(_) => todo!(),
            Error::Alpm(_) => todo!(),
            Error::TerminalLogger(_) => todo!(),
            Error::PacConf(_) => todo!(),
            Error::Env(_) => todo!(),
            Error::Conf(_) => todo!(),
            Error::Check(_) => todo!(),
            Error::Open(_) => todo!(),
            Error::Stats(_) => todo!(),
        }
    }
}
