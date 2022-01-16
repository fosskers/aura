use std::ffi::OsStr;
use std::process::Command;

pub enum Error {
    Io(std::io::Error),
    Misc,
}

impl From<std::io::Error> for Error {
    fn from(v: std::io::Error) -> Self {
        Self::Io(v)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "A shell call to Pacman gave a non-zero exit code.")
    }
}

/// Make a shell call to `pacman`.
pub(crate) fn pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let es = Command::new("pacman").args(args).status()?;
    es.success().then(|| ()).ok_or(Error::Misc)
}

/// Make an elevated shell call to `pacman`.
pub(crate) fn sudo_pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let es = Command::new("sudo").arg("pacman").args(args).status()?;
    es.success().then(|| ()).ok_or(Error::Misc)
}
