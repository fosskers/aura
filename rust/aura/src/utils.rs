//! Various utility functions.

use crate::error::Nested;
use crate::localization::Localised;
use colored::{ColoredString, Colorize};
use i18n_embed_fl::fl;
use rustyline::Editor;
use std::io::Write;
use std::iter::Peekable;
use std::path::Path;
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

/// Produce a proper UTF-8 `String` from a `Path`-like type.
pub(crate) trait PathStr {
    fn utf8(&self) -> String;
}

impl<T> PathStr for T
where
    T: AsRef<Path>,
{
    fn utf8(&self) -> String {
        self.as_ref().display().to_string()
    }
}

/// A helper for commands like `-Ai`, `-Ci`, etc.
pub(crate) fn info<W>(
    w: &mut W,
    lang: LanguageIdentifier,
    pairs: &[(&str, ColoredString)],
) -> Result<(), std::io::Error>
where
    W: Write,
{
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
pub(crate) fn prompt(msg: &str) -> Option<()> {
    let mut rl = Editor::<()>::new();
    let line = rl.readline(msg).ok()?;

    (line.is_empty() || line == "y" || line == "Y").then(|| ())
}

/// Prompt the user for a numerical selection.
pub(crate) fn select(msg: &str, max: usize) -> Result<usize, rustyline::error::ReadlineError> {
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

pub(crate) struct SudoError;

impl Nested for SudoError {
    fn nested(&self) {}
}

impl Localised for SudoError {
    fn localise(&self, fll: &i18n_embed::fluent::FluentLanguageLoader) -> String {
        fl!(fll, "err-sudo")
    }
}

/// Escalate the privileges of the Aura process, if necessary.
pub(crate) fn sudo() -> Result<(), SudoError> {
    sudo::escalate_if_needed().map_err(|_| SudoError).void()
}

/// A wrapper around [`time::Date`] to supply some trait instances.
#[derive(Debug)]
pub(crate) struct Date(pub time::Date);

impl FromStr for Date {
    type Err = time::error::Parse;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        time::Date::parse(
            s,
            &time::macros::format_description!("[year]-[month]-[day]"),
        )
        .map(Date)
    }
}

/// An [`Iterator`] that knows if the current iteration step is the last one.
/// Utilizes [`Peekable`] under the hood, so note that this forces the iteration
/// of the next element.
pub(crate) struct Finished<I>
where
    I: Iterator,
{
    iter: Peekable<I>,
}

impl<I> Finished<I>
where
    I: Iterator,
{
    /// Construct a new `Marked` iterator.
    pub(crate) fn new(iter: I) -> Self {
        Finished {
            iter: iter.peekable(),
        }
    }
}

impl<I> Iterator for Finished<I>
where
    I: Iterator,
{
    type Item = Iteration<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(t) if self.iter.peek().is_none() => Some(Iteration::Final(t)),
            Some(t) => Some(Iteration::Middle(t)),
            None => None,
        }
    }
}

/// How far along an iteration are we?
pub(crate) enum Iteration<T> {
    Middle(T),
    Final(T),
}

impl<T> Iteration<T> {
    pub(crate) fn inner(self) -> T {
        match self {
            Iteration::Middle(t) => t,
            Iteration::Final(t) => t,
        }
    }

    pub(crate) fn is_last(&self) -> bool {
        matches!(self, Iteration::Final(_))
    }
}
