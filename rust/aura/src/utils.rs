//! Various utility functions.

use crate::error::Error;
use colored::{ColoredString, Colorize};
use rustyline::Editor;
use std::ffi::OsStr;
use std::io::Write;
use std::process::Command;
use std::str::FromStr;
use unic_langid::LanguageIdentifier;

/// Injection of the `void` method into [`Result`], which is a common shorthand
/// for "forgetting" the internal return value of a `Result`. Note that this
/// also automatically lifts the Error type via [`From`], as it is intended as
/// the final line of a function where `?` doesn't work.
///
/// We assume that only `Result` is ever going to implement this trait. If Rust
/// had Higher-kinded Types, this would be much simpler and could be applied to
/// more types.
pub(crate) trait ResultVoid<E, R> {
    fn void(self) -> Result<(), R>
    where
        R: From<E>;
}

impl<T, E, R> ResultVoid<E, R> for Result<T, E> {
    fn void(self) -> Result<(), R>
    where
        R: From<E>,
    {
        match self {
            Ok(_) => Ok(()),
            Err(e) => Err(From::from(e)),
        }
    }
}

/// A helper for commands like `-Ai`, `-Ci`, etc.
pub(crate) fn info<W: Write>(
    w: &mut W,
    lang: LanguageIdentifier,
    pairs: &[(&str, ColoredString)],
) -> Result<(), std::io::Error> {
    // Different languages consume varying char widths in the terminal.
    //
    // TODO Account for other languages (Chinese, and what else?)
    let m = if lang.language == "ja" { 2 } else { 1 };

    // The longest field.
    let l = pairs
        .iter()
        .map(|(l, _)| l.chars().count())
        .max()
        .unwrap_or(0);

    for (lbl, value) in pairs {
        writeln!(w, "{}{:w$} : {}", lbl.bold(), "", value, w = pad(m, l, lbl))?;
    }

    Ok(())
}

fn pad(mult: usize, longest: usize, s: &str) -> usize {
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

/// Escalate the privileges of the Aura process, if necessary.
pub(crate) fn sudo() -> Result<(), Error> {
    sudo::escalate_if_needed().map_err(|_| Error::Sudo).void()
}
