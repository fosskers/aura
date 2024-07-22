//! Various utility functions.

use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;
use colored::ColoredString;
use colored::Colorize;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use karen::RunningAs;
use nonempty_collections::NEVec;
use rustyline::Editor;
use std::io::Write;
use std::iter::Peekable;
use std::path::Path;
use std::process::Command;
use std::str::FromStr;
use unic_langid::LanguageIdentifier;

/// An empty array to satisfy typechecking in a few places around the codebase.
pub(crate) const NOTHING: [&str; 0] = [];

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
    let m = match lang.language.as_str() {
        "ja" | "ko" | "zh" => 2,
        _ => 1,
    };

    // The longest field.
    let l = pairs
        .iter()
        .map(|(l, _)| l.as_ref().chars().count())
        .max()
        .unwrap_or(0);

    for (lbl, value) in pairs {
        let lbl = lbl.as_ref();
        writeln!(w, "{}{:w$} : {}", lbl.bold(), "", value, w = pad(m, l, lbl))?;
    }

    Ok(())
}

fn pad(mult: usize, longest: usize, s: &str) -> usize {
    mult * (longest - s.chars().count())
}

/// Prompt the user for confirmation.
pub(crate) fn prompt(fll: &FluentLanguageLoader, msg: &str) -> Option<()> {
    let mut rl = Editor::<(), _>::new().ok()?;
    let line = rl.readline(msg).ok()?;
    let accept_small = fl!(fll, "proceed-affirmative");
    let accept_large = fl!(fll, "proceed-affirmative-alt");

    (line.is_empty() || line == accept_small || line == accept_large).then_some(())
}

/// Prompt the user for a numerical selection.
pub(crate) fn select(msg: &str, max: usize) -> Result<usize, rustyline::error::ReadlineError> {
    let mut rl = Editor::<(), _>::new()?;

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
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        fl!(fll, "err-sudo")
    }
}

/// Escalate the privileges of the entire Aura process, if necessary.
pub(crate) fn sudo(env: &Env) -> Result<(), SudoError> {
    karen::builder()
        .wrapper(env.sudo())
        .with_env(&["LANG", "EDITOR"])
        .map_err(|_| SudoError)
        .void()
}

/// Is Aura being run by the root user?
pub(crate) fn is_root_user() -> bool {
    matches!(karen::check(), RunningAs::Root)
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

/// The lines out output from some shell command.
///
/// Slightly wasteful in terms of allocations, so should be used only for
/// commands whose output is known not to be that long.
pub(crate) fn cmd_lines(cmd: &str, args: &[&str]) -> Option<NEVec<String>> {
    Command::new(cmd)
        .args(args)
        .output()
        .ok()
        .map(|o| o.stdout)
        .and_then(|stdout| String::from_utf8(stdout).ok())
        .and_then(|s| {
            let v = s
                .lines()
                .map(|line| line.trim().to_string())
                .collect::<Vec<_>>();

            NEVec::from_vec(v)
        })
}
