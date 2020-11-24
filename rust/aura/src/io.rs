//! Utilities for user output.

use colored::*;

// TODO Make this a macro.
/// Print a coloured Aura message.
pub fn aln(msg: &str) {
    println!("{} {} {}", "aura".bold(), "::".cyan().bold(), msg.bold());
}

/// Format a message according to the Aura style.
pub fn a(msg: &str) -> String {
    format!("{} {} {}", "aura".bold(), "::".cyan().bold(), msg.bold())
}
