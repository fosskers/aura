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
