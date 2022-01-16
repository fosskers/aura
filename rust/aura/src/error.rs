//! All errors that can occur in the Aura executable.

/// Error type for all issues that can occur in the Aura library or executable.
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
}

impl From<crate::pacman::Error> for Error {
    fn from(v: crate::pacman::Error) -> Self {
        Self::Pacman(v)
    }
}

impl From<crate::command::snapshot::Error> for Error {
    fn from(v: crate::command::snapshot::Error) -> Self {
        Self::B(v)
    }
}

impl From<crate::command::orphans::Error> for Error {
    fn from(v: crate::command::orphans::Error) -> Self {
        Self::O(v)
    }
}

impl From<crate::command::cache::Error> for Error {
    fn from(v: crate::command::cache::Error) -> Self {
        Self::C(v)
    }
}

impl From<crate::dirs::Error> for Error {
    fn from(v: crate::dirs::Error) -> Self {
        Self::Dirs(v)
    }
}

impl From<crate::command::aur::Error> for Error {
    fn from(v: crate::command::aur::Error) -> Self {
        Self::A(v)
    }
}

impl From<log::SetLoggerError> for Error {
    fn from(error: log::SetLoggerError) -> Self {
        Error::Log(error)
    }
}

impl From<i18n_embed::I18nEmbedError> for Error {
    fn from(error: i18n_embed::I18nEmbedError) -> Self {
        Error::I18n(error)
    }
}

impl From<pacmanconf::Error> for Error {
    fn from(error: pacmanconf::Error) -> Self {
        Error::PacConf(error)
    }
}

impl From<alpm::Error> for Error {
    fn from(error: alpm::Error) -> Self {
        Error::Alpm(error)
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IO(error)
    }
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
        }
    }
}
