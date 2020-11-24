//! Utilities for user output.

use crate::error::Error;
use std::io::Write;
use termcolor::{Buffer, Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

/// Output a line of green text.
pub fn agreenln(msg: &str) -> Result<(), Error> {
    aurad(Color::Green, true, msg)
}

/// Output a line of yellow text.
pub fn ayellowln(msg: &str) -> Result<(), Error> {
    aurad(Color::Yellow, true, msg)
}

/// Output a line of cyan text.
pub fn cyan(msg: &str) -> Result<(), Error> {
    coloured(Color::Cyan, msg)
}

/// Output a line of magenta text.
pub fn magenta(msg: &str) -> Result<(), Error> {
    coloured(Color::Magenta, msg)
}

/// Doesn't insert a newline.
fn coloured(colour: Color, msg: &str) -> Result<(), Error> {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(colour)))
        .map_err(Error::IO)?;
    write!(&mut stdout, "{}", msg).map_err(Error::IO)?;
    stdout.reset().map_err(Error::IO)
}

/// Optionally inserts a newline.
fn aurad(colour: Color, nl: bool, msg: &str) -> Result<(), Error> {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    write!(&mut stdout, "aura").map_err(Error::IO)?;
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
        .map_err(Error::IO)?;
    write!(&mut stdout, " :: ").map_err(Error::IO)?;
    stdout
        .set_color(ColorSpec::new().set_fg(Some(colour)))
        .map_err(Error::IO)?;
    if nl {
        writeln!(&mut stdout, "{}", msg).map_err(Error::IO)?;
    } else {
        write!(&mut stdout, "{}", msg).map_err(Error::IO)?;
    }
    stdout.reset().map_err(Error::IO)
}
