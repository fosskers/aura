use clap::{crate_version, AppSettings, Clap};
use simplelog::LevelFilter;

#[derive(Clap, Debug)]
#[clap(version = crate_version!(),
       author = "Colin Woodbury",
       about = "Install and manage Arch Linux and AUR packages",
       setting = AppSettings::VersionlessSubcommands,
       setting = AppSettings::DisableHelpSubcommand)]
pub struct Args {
    /// Output in English.
    #[clap(group = "lang", long, global = true)]
    english: bool,
    /// Output in Japanese.
    #[clap(group = "lang", long, global = true, alias = "日本語")]
    japanese: bool,
    /// Less verbose output.
    #[clap(long, short, global = true)]
    quiet: bool,
    /// Minimum level of log messages to display.
    #[clap(long, value_name = "LEVEL", possible_values = &["debug", "info", "warn", "error"], global = true)]
    log_level: Option<LevelFilter>,
    #[clap(subcommand)]
    subcmd: SubCmd,
}

#[derive(Clap, Debug)]
pub enum SubCmd {
    // --- Pacman Commands --- //
    Database,
    Files,
    Query,
    Remove,
    Sync(Sync),
    TestDeps,
    Upgrade,
    // --- Aura Commands --- //
    // Completions(Completions),
}

/// Synchronize packages.
#[derive(Clap, Debug)]
#[clap(short_flag = 'S', long_flag = "sync")]
pub struct Sync {
    #[clap(long, short, about = "Search for packages via REGEX.")]
    search: bool,
    #[clap(long, short, about = "Look up a specific package.")]
    info: bool,
    #[clap(about = "Packages to search/install.")]
    packages: Vec<String>,
}
