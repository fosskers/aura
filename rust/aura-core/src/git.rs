//! Generalized `git` interaction.

use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};

// FIXME This seems to succeed for non-existant repos!
/// Perform a shallow clone frrom a given repository url, and save it to a given
/// `Path` on the filesystem.
pub fn shallow_clone(url: &Path, target: &Path) -> Result<ExitStatus, std::io::Error> {
    Command::new("git")
        .arg("clone")
        .arg("--depth=1")
        .arg(url)
        .arg(target)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
}

/// Given a `Path` that is known to be a Git repository, visit it and pull the
/// latest commits.
///
/// Uses the `--ff-only` merge strategy, so the commit history can't have
/// diverged from the `origin` or this will fail.
pub fn pull(clone_dir: &Path) -> Result<ExitStatus, std::io::Error> {
    Command::new("git")
        .arg("pull")
        .arg("--quiet")
        .arg("--ff-only")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .current_dir(clone_dir)
        .status()
}
