//! All errors that can occur in the Aura executable.

/// Error type for all issues that can occur in the Aura library or executable.
pub(crate) enum Error {
    A(crate::command::aur::Error),
    Dirs(crate::dirs::Error),
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
    /// An error from `chrono`.
    Chrono(chrono::ParseError),
    /// A JSON (de)serialization error.
    Json(serde_json::Error),
    /// An error calling the aurweb API.
    Aur(String),
    /// The said "no" at some prompt.
    Rejected,
    /// None of the packages specified by the user actually exist.
    NoneExist,
    /// A non-zero exit code was returned from a call to Pacman.
    Pacman,
    /// We were unable to escalate the process with `sudo`.
    Sudo,
    // /// A file IO target already exists.
    // FileConflict,
    /// A miscellaneous shell call failed.
    MiscShell,
    /// A silent, miscellaneous error.
    ///
    /// In theory any relevant error messages have already been localized and
    /// shown to the user.
    Silent,
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

impl From<rustyline::error::ReadlineError> for Error {
    fn from(error: rustyline::error::ReadlineError) -> Self {
        Error::RustyLine(error)
    }
}

impl From<chrono::ParseError> for Error {
    fn from(error: chrono::ParseError) -> Self {
        Error::Chrono(error)
    }
}

impl From<std::env::VarError> for Error {
    fn from(error: std::env::VarError) -> Self {
        Error::Env(error)
    }
}

impl From<curl::Error> for Error {
    fn from(error: curl::Error) -> Self {
        Error::Curl(error)
    }
}

impl From<serde_json::Error> for Error {
    fn from(error: serde_json::Error) -> Self {
        Error::Json(error)
    }
}

impl From<raur_curl::Error> for Error {
    fn from(error: raur_curl::Error) -> Self {
        match error {
            raur_curl::Error::Curl(e) => Error::Curl(e),
            raur_curl::Error::Serde(e) => Error::Json(e),
            raur_curl::Error::Aur(e) => Error::Aur(e),
        }
    }
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
            Error::Chrono(e) => write!(f, "{}", e),
            Error::Json(e) => write!(f, "{}", e),
            Error::Aur(e) => write!(f, "{}", e),
            Error::Rejected => write!(f, "The user said no."),
            Error::NoneExist => write!(f, "None of those packages exist."),
            Error::Pacman => write!(f, "A shell call to Pacman gave a non-zero exit code."),
            Error::Sudo => write!(f, "Unable to escalate via sudo."),
            Error::MiscShell => write!(f, "A miscellaneous shell call failed."),
            // Error::FileConflict => write!(f, "The given file target already exists."),
            Error::Silent => write!(f, ""),
            Error::A(e) => write!(f, "{}", e),
            Error::Dirs(e) => write!(f, "{}", e),
        }
    }
}
