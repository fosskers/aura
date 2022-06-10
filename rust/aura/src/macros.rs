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
        format!("{} {} {}", "aura".bold(), "::".cyan().bold(), $msg.bold())
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

#[macro_export]
/// Detect an executable or warn if it's missing.
///
/// Used by the `check` module.
macro_rules! executable {
    ($fll:expr, $exec:expr, $msg:expr, $($arg:expr),*) => {
        let good = which::which($exec).is_ok();
        let symb = if good { crate::command::check::GOOD.green() } else { crate::command::check::BAD.red() };
        println!(
            "  [{}] {}",
            symb,
            i18n_embed_fl::fl!($fll, $msg, $($arg)*)
        );

        if !good {
            let msg = fl!($fll, "check-missing-exec", exec = $exec.cyan().bold().to_string());
            println!("      └─ {}", msg);
        }
    };
    ($fll:expr, $exec:expr, $msg:expr) => {
        let good = which::which($exec).is_ok();
        let symb = if good { crate::command::check::GOOD.green() } else { crate::command::check::BAD.red() };
        println!(
            "  [{}] {}",
            symb,
            i18n_embed_fl::fl!($fll, $msg)
        );

        if !good {
            let msg = fl!($fll, "check-missing-exec", exec = $exec.cyan().bold().to_string());
            println!("      └─ {}", msg);
        }
    };
}

#[macro_export]
/// Ask for permission to proceed, but with a custom message.
macro_rules! proceed {
    ($fll:expr, $msg:expr) => {{
        let foo = format!(
            "{} {} ",
            i18n_embed_fl::fl!($fll, $msg),
            i18n_embed_fl::fl!($fll, "proceed-yes")
        );
        crate::utils::prompt(&$crate::a!(foo))
    }};
}
