//! Generalized `git` interaction.

use std::path::Path;
use std::process::{Command, Stdio};

use log::debug;

/// A git-related error.
pub enum Error {
    /// Some IO action failed.
    Io(std::io::Error),
    /// A git clone failed.
    Clone,
    /// A git pull failed.
    Pull,
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
            Error::Clone => write!(f, "A git clone failed."),
            Error::Pull => write!(f, "A git pull failed."),
        }
    }
}

// FIXME This seems to succeed for non-existant repos!
/// Perform a shallow clone frrom a given repository url, and save it to a given
/// `Path` on the filesystem.
pub fn shallow_clone(url: &Path, target: &Path) -> Result<(), Error> {
    debug!("Cloning {}", url.display());

    Command::new("git")
        .arg("clone")
        .arg("--depth=1")
        .arg(url)
        .arg(target)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Clone)
}

/// Given a `Path` that is known to be a Git repository, visit it and pull the
/// latest commits.
///
/// Uses the `--ff-only` merge strategy, so the commit history can't have
/// diverged from the `origin` or this will fail.
pub fn pull(dir: &Path) -> Result<(), Error> {
    Command::new("git")
        .arg("pull")
        .arg("--quiet")
        .arg("--ff-only")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .current_dir(dir)
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Pull)
}
