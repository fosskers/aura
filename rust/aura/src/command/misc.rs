//! Miscellaneous functionality.

use std::path::Path;

/// Expected location of the `bat` executable if installed from official repos.
const BAT: &str = "/bin/bat";

/// Expected location of the `less` executable.
const LESS: &str = "/bin/less";

/// Expected location of the `ripgrep` executable.
const RIPGREP: &str = "/bin/rg";

/// Expected location of the `grep` executable.
const GREP: &str = "/bin/grep";

/// Display the locales that Aura has been translated to.
pub fn languages() {
    for lang in crate::localization::available_languages() {
        println!("{}", lang);
    }
}

/// A complete path to a file viewer program like `less`.
pub fn viewer() -> &'static str {
    let bat = Path::new(BAT);
    if bat.exists() {
        BAT
    } else {
        LESS
    }
}

/// A complete path to a file searcher program like `grep`, along with any extra
/// arguments needed to affect the exact output.
pub fn searcher() -> (&'static str, &'static [&'static str]) {
    let rg = Path::new(RIPGREP);
    if rg.exists() {
        (RIPGREP, &["-N"])
    } else {
        (GREP, &[])
    }
}
