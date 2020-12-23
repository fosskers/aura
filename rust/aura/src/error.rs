//! All errors that can occur in the Aura executable.

/// Error type for all issues that can occur in the Aura library or executable.
#[derive(Debug)]
pub(crate) enum Error {
    /// Some IO error, say from reading a file or sending a command to the
    /// shell.
    IO(std::io::Error),
    /// An error occurred within the `alpm` C code.
    Alpm(alpm::Error),
    /// The user exited a prompt in some way, say by CTRL+C.
    RustyLine(rustyline::error::ReadlineError),
    /// An error occurred when reading the localizations upon startup.
    I18n(i18n_embed::I18nEmbedError),
    /// An error during logger initialization.
    Log(log::SetLoggerError),
    /// An error reading an environment variable.
    Env(std::env::VarError),
    /// An error parsing `pacman.conf`.
    PacConf(pacmanconf::Error),
    /// An error from `curl`.
    Curl(curl::Error),
    /// A boxed dynamic error.
    Boxed(Box<dyn std::error::Error>),
    /// The said "no" at some prompt.
    Rejected,
    /// None of the packages specified by the user actually exist.
    NoneExist,
    /// A non-zero exit code was returned from a call to Pacman.
    PacmanError,
    /// A file IO target already exists.
    FileConflict,
    /// A silent, miscellaneous error.
    ///
    /// In theory any relevant error messages have already been localized and
    /// shown to the user.
    Silent,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::IO(e) => write!(f, "{}", e),
            Error::Alpm(e) => write!(f, "{}", e),
            Error::RustyLine(e) => write!(f, "{}", e),
            Error::I18n(e) => write!(f, "{}", e),
            Error::Log(e) => write!(f, "{}", e),
            Error::Env(e) => write!(f, "{}", e),
            Error::PacConf(e) => write!(f, "{}", e),
            Error::Curl(e) => write!(f, "{}", e),
            Error::Boxed(e) => write!(f, "{}", e),
            Error::Rejected => write!(f, "The user said no."),
            Error::NoneExist => write!(f, "None of those packages exist."),
            Error::PacmanError => write!(f, "A shell call to Pacman gave a non-zero exit code."),
            Error::FileConflict => write!(f, "The given file target already exists."),
            Error::Silent => write!(f, ""),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::IO(e) => Some(e),
            Error::Alpm(e) => Some(e),
            Error::RustyLine(e) => Some(e),
            Error::I18n(e) => Some(e),
            Error::Log(e) => Some(e),
            Error::Env(e) => Some(e),
            Error::PacConf(e) => Some(e),
            Error::Curl(e) => Some(e),
            Error::Boxed(_) => None, // TODO `Some` gives a warning.
            Error::Rejected => None,
            Error::NoneExist => None,
            Error::PacmanError => None,
            Error::FileConflict => None,
            Error::Silent => None,
        }
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

impl From<rustyline::error::ReadlineError> for Error {
    fn from(error: rustyline::error::ReadlineError) -> Self {
        Error::RustyLine(error)
    }
}

impl From<std::env::VarError> for Error {
    fn from(error: std::env::VarError) -> Self {
        Error::Env(error)
    }
}

impl From<Box<dyn std::error::Error>> for Error {
    fn from(error: Box<dyn std::error::Error>) -> Self {
        Error::Boxed(error)
    }
}

impl From<curl::Error> for Error {
    fn from(error: curl::Error) -> Self {
        Error::Curl(error)
    }
}
