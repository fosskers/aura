//! Generalized `git` interaction.

use crate::Apply;
use from_variants::FromVariants;
use log::debug;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// A git-related error.
#[derive(Debug, FromVariants)]
pub enum Error {
    /// Some IO action failed.
    Io(std::io::Error),
    /// A git clone failed.
    #[from_variants(skip)]
    Clone(PathBuf),
    /// A git pull failed.
    #[from_variants(skip)]
    Pull(PathBuf),
    /// A git diff failed.
    #[from_variants(skip)]
    Diff(PathBuf),
    /// Converting a git hash to a Rust string failed.
    ReadHash(std::string::FromUtf8Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "{}", e),
            Error::Clone(p) => write!(f, "A git clone failed: {}", p.display()),
            Error::Pull(p) => write!(f, "A git pull failed: {}", p.display()),
            Error::ReadHash(e) => write!(f, "Reading a git hash into Rust failed: {e}"),
            Error::Diff(p) => write!(f, "A git diff failed: {}", p.display()),
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
        .ok_or_else(|| Error::Clone(url.to_path_buf()))
}

/// Given a `Path` that is known to be a Git repository, visit it and pull the
/// latest commits.
///
/// Uses the `--ff-only` merge strategy, so the commit history can't have
/// diverged from the `origin` or this will fail.
pub fn pull(dir: &Path) -> Result<(), Error> {
    debug!("Pulling {}", dir.display());

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
        .ok_or_else(|| Error::Pull(dir.to_path_buf()))
}

/// Given a `Path` to a known local git repo, find out the hash of its latest
/// commit.
pub fn hash(dir: &Path) -> Result<String, Error> {
    debug!("git rev-parse: {}", dir.display());

    Command::new("git")
        .arg("rev-parse")
        .arg("HEAD")
        .current_dir(dir)
        .output()?
        .stdout
        .apply(String::from_utf8)
        .map_err(Error::ReadHash)
}

/// Display the diff between `master` and a given hash.
pub fn diff(dir: &Path, hash: &str) -> Result<(), Error> {
    debug!("git diff: {}", dir.display());

    Command::new("git")
        .arg("diff")
        .arg(hash)
        .current_dir(dir)
        .status()?
        .success()
        .then(|| ())
        .ok_or_else(|| Error::Diff(dir.to_path_buf()))
}
