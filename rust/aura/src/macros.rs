//! Macro definitions.

#[macro_export]
/// Print a coloured Aura message.
macro_rules! aln {
    ($msg:expr) => {
        println!("{} {} {}", "aura".bold(), "::".cyan().bold(), $msg.bold());
    };
}

#[macro_export]
/// Format a message according to the Aura style.
macro_rules! a {
    ($msg:expr) => {
        format!("{} {} {}", "aura".bold(), "::".cyan().bold(), $msg.bold());
    };
}

#[macro_export]
/// Print a localized Aura message, given some Fluent tag.
macro_rules! aura {
    ($fll:expr, $msg:expr, $($arg:expr),*) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg, $($arg)*));
    };
    ($fll:expr, $msg:expr) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg));
    };
}

#[macro_export]
/// Print a localized Aura message in green, given some Fluent tag.
macro_rules! green {
    ($fll:expr, $msg:expr, $($arg:expr),*) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg, $($arg)*).green());
    };
    ($fll:expr, $msg:expr) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg).green());
    };
}

#[macro_export]
/// Print a localized Aura message in yellow, given some Fluent tag.
macro_rules! yellow {
    ($fll:expr, $msg:expr, $($arg:expr),*) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg, $($arg)*).yellow());
    };
    ($fll:expr, $msg:expr) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg).yellow());
    };
}

#[macro_export]
/// Print a localized Aura message in red, given some Fluent tag.
macro_rules! red {
    ($fll:expr, $msg:expr, $($arg:expr),*) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg, $($arg)*).red());
    };
    ($fll:expr, $msg:expr) => {
        $crate::aln!(i18n_embed_fl::fl!($fll, $msg).red());
    };
}
