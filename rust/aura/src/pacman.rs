//! Sugar for interacting with Pacman.

use std::ffi::OsStr;
use std::process::Command;

pub enum Error {
    Io(std::io::Error),
    Install,
    Misc,
}

impl From<std::io::Error> for Error {
    fn from(v: std::io::Error) -> Self {
        Self::Io(v)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{}", e),
            Error::Install => write!(f, "pacman -U failed"),
            Error::Misc => write!(f, "A call to pacman gave a non-zero exit code"),
        }
    }
}

/// Make a shell call to `pacman`.
pub(crate) fn pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    Command::new("pacman")
        .args(args)
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Misc)
}

/// Make an elevated shell call to `pacman`.
pub(crate) fn sudo_pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    Command::new("sudo")
        .arg("pacman")
        .args(args)
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Misc)
}

/// Call `sudo pacman -U`.
pub(crate) fn pacman_install<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    Command::new("sudo")
        .arg("pacman")
        .arg("-U")
        .args(args)
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Install)
}
