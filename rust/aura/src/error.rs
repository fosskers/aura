//! All errors that can occur in the Aura executable.

/// Error type for all issues that can occur in the Aura library or executable.
#[derive(Debug)]
pub enum Error {
    /// Some IO error, say from reading a file or sending a command to the
    /// shell.
    IO(std::io::Error),
    /// Some error from the ALPM utilities.
    Arch(aura_arch::Error),
    /// An error occurred within the `alpm` C code.
    Alpm(alpm::Error),
    /// The user exited a prompt in some way, say by CTRL+C.
    RustyLine(rustyline::error::ReadlineError),
    /// An error occurred when reading the localizations upon startup.
    I18n(i18n_embed::I18nEmbedError),
    /// An error during logger initialization.
    Log(simplelog::TermLogError),
    /// An error reading an environment variable.
    Env(std::env::VarError),
    /// An error parsing `pacman.conf`.
    PacConf(pacmanconf::Error),
    /// The said "no" at some prompt.
    Rejected,
    /// None of the packages specified by the user actually exist.
    NoneExist,
    /// A non-zero exit code was returned from a call to Pacman.
    PacmanError,
    /// A silent, miscellaneous error.
    ///
    /// In theory any relevant error messages have already been localized and
    /// shown to the user.
    Silent,
}

impl From<simplelog::TermLogError> for Error {
    fn from(error: simplelog::TermLogError) -> Self {
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
