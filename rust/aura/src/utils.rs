//! Various utility functions.

use crate::error::Error;
use colored::{ColoredString, Colorize};
use rustyline::Editor;
use std::ffi::OsStr;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;
use unic_langid::LanguageIdentifier;

/// A helper for commands like `-Ai`, `-Ci`, etc.
pub(crate) fn info<W, S>(
    w: &mut W,
    lang: LanguageIdentifier,
    pairs: &[(S, ColoredString)],
) -> Result<(), std::io::Error>
where
    W: Write,
    S: AsRef<str>,
{
    // Different languages consume varying char widths in the terminal.
    //
    // TODO Account for other languages (Chinese, and what else?)
    let m = if lang.language == "ja" { 2 } else { 1 };

    // The longest field.
    let l = pairs
        .iter()
        .map(|(l, _)| l.as_ref().chars().count())
        .max()
        .unwrap_or(0);

    for (label, value) in pairs {
        let lbl = label.as_ref();
        writeln!(w, "{}{:w$} : {}", lbl.bold(), "", value, w = pad(m, l, lbl))?;
    }

    writeln!(w) // A blank line.
}

// TODO Drop `pub` once other functions use `info` above.
pub(crate) fn pad(mult: usize, longest: usize, s: &str) -> usize {
    mult * (longest - s.chars().count())
}

// TODO Localize the acceptance chars.
/// Prompt the user for confirmation.
pub(crate) fn prompt(msg: &str) -> Result<(), Error> {
    let mut rl = Editor::<()>::new();
    match rl.readline(msg) {
        Ok(line) if line.is_empty() || line == "y" || line == "Y" => Ok(()),
        Ok(_) => Err(Error::Rejected),
        Err(e) => Err(Error::RustyLine(e)),
    }
}

/// Prompt the user for a numerical selection.
pub(crate) fn select(msg: &str, max: usize) -> Result<usize, Error> {
    let mut rl = Editor::<()>::new();

    loop {
        let raw = rl.readline(msg)?;

        if let Ok(num) = usize::from_str(&raw) {
            if max >= num {
                return Ok(num);
            }
        }
    }
}

/// Make a shell call to `pacman`.
pub(crate) fn pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    match Command::new("pacman").args(args).status() {
        Err(e) => Err(Error::IO(e)),
        Ok(es) if es.success() => Ok(()),
        Ok(_) => Err(Error::Pacman),
    }
}

/// Make an elevated shell call to `pacman`.
pub(crate) fn sudo_pacman<I, S>(args: I) -> Result<(), Error>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    match Command::new("sudo").arg("pacman").args(args).status() {
        Err(e) => Err(Error::IO(e)),
        Ok(es) if es.success() => Ok(()),
        Ok(_) => Err(Error::Pacman),
    }
}

/// Fetch the path value of `$XDG_CACHE_HOME` or provide its default according
/// to the specification:
///
/// > `$XDG_CACHE_HOME` defines the base directory relative to which user specific
/// > non-essential data files should be stored. If `$XDG_CACHE_HOME` is either not
/// > set or empty, a default equal to `$HOME/.cache` should be used.
fn xdg_cache() -> Result<PathBuf, std::env::VarError> {
    std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|_| {
            std::env::var("HOME").map(|h| {
                let mut path = PathBuf::from(h);
                path.push(".cache");
                path
            })
        })
}

/// The full path to the Aura cache.
pub(crate) fn aura_xdg_cache() -> Result<PathBuf, std::env::VarError> {
    let mut cache = xdg_cache()?;
    cache.push("aura");
    Ok(cache)
}
