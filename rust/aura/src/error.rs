//! All errors that can occur in the Aura executable.

use from_variants::FromVariants;

/// Error type for all issues that can occur in the Aura library or executable.
#[derive(FromVariants)]
pub(crate) enum Error {
    A(crate::command::aur::Error),
    B(crate::command::snapshot::Error),
    C(crate::command::cache::Error),
    O(crate::command::orphans::Error),
    Dirs(crate::dirs::Error),
    /// A non-zero exit code was returned from a call to Pacman.
    Pacman(crate::pacman::Error),
    /// Some IO error, say from reading a file or sending a command to the
    /// shell.
    IO(std::io::Error),
    /// An error occurred within the `alpm` C code.
    Alpm(alpm::Error),
    /// An error occurred when reading the localizations upon startup.
    I18n(i18n_embed::I18nEmbedError),
    /// An error during logger initialization.
    Log(log::SetLoggerError),
    /// An error parsing `pacman.conf`.
    PacConf(pacmanconf::Error),
    Env(crate::env::Error),
    Conf(crate::conf::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::IO(e) => write!(f, "{}", e),
            Error::Alpm(e) => write!(f, "{}", e),
            Error::I18n(e) => write!(f, "{}", e),
            Error::Log(e) => write!(f, "{}", e),
            Error::PacConf(e) => write!(f, "{}", e),
            Error::Pacman(e) => write!(f, "{}", e),
            Error::Dirs(e) => write!(f, "{}", e),
            Error::A(e) => write!(f, "{}", e),
            Error::B(e) => write!(f, "{}", e),
            Error::C(e) => write!(f, "{}", e),
            Error::O(e) => write!(f, "{}", e),
            Error::Env(e) => write!(f, "{}", e),
            Error::Conf(e) => write!(f, "{e}"),
        }
    }
}
