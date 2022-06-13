//! Sugar for interacting with Pacman.

use from_variants::FromVariants;
use std::ffi::OsStr;
use std::process::Command;

#[derive(FromVariants)]
pub enum Error {
    Io(std::io::Error),
    InstallFromTarball,
    InstallFromRepos,
    Misc,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{}", e),
            Error::InstallFromTarball => write!(f, "pacman -U failed"),
            Error::InstallFromRepos => write!(f, "pacman -S failed"),
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
pub(crate) fn sudo_pacman<I, J, S, T>(command: &str, flags: I, args: J) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    J: IntoIterator<Item = T>,
    S: AsRef<OsStr>,
    T: AsRef<OsStr>,
{
    Command::new("sudo")
        .arg("pacman")
        .arg(command)
        .args(flags)
        .args(args)
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Misc)
}

/// Make an elevated shell call to `pacman`, passing all arguments to pacman as-is.
pub(crate) fn sudo_pacman_batch<I, S>(args: I) -> Result<(), Error>
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
pub(crate) fn pacman_install_from_tarball<I, J, S, T>(flags: I, args: J) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    J: IntoIterator<Item = T>,
    S: AsRef<OsStr>,
    T: AsRef<OsStr>,
{
    sudo_pacman("-U", flags, args).map_err(|_| Error::InstallFromTarball)
}

/// Call `sudo pacman -S`.
pub(crate) fn pacman_install_from_repos<I, J, S, T>(flags: I, args: J) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    J: IntoIterator<Item = T>,
    S: AsRef<OsStr>,
    T: AsRef<OsStr>,
{
    sudo_pacman("-S", flags, args).map_err(|_| Error::InstallFromRepos)
}
