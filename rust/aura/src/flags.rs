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
    /// Minimum level of Aura log messages to display.
    #[clap(long, value_name = "level", possible_values = &["debug", "info", "warn", "error"], global = true)]
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
    AurSync,
    Backup,
    Cache,
    Log,
    Orphans,
    Analysis,
    Languages,
    ViewConf,
    Extra, // TODO completions, dot output, etc?
}

/// Synchronize packages.
#[derive(Clap, Debug)]
#[clap(short_flag = 'S', long_flag = "sync")]
pub struct Sync {
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
    /// Remove old packages from cache directory (-cc for all).
    #[clap(long, short)]
    clean: bool,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd')]
    nodeps: bool,
    /// View all members of a package group (-gg to view all groups and members).
    #[clap(long, short)]
    groups: bool,
    /// View package information (-ii for extended information).
    #[clap(long, short, about = "Look up a specific package.")]
    info: bool,
    /// View a list of packages in a repo.
    #[clap(long, short)]
    list: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short)]
    print: bool,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path")]
    root: Option<String>,
    /// Search remote repositories for matchings strings.
    #[clap(long, short)]
    search: bool,
    /// Upgrade installed packages (-uu enables downgrades).
    #[clap(long, short = 'u')]
    sysupgrade: bool,
    /// Be verbose.
    #[clap(long, short)]
    verbose: bool,
    /// Download packages but do not install/upgrade anything.
    #[clap(long, short = 'w')]
    downloadonly: bool,
    /// Download fresh package databases from the server (-yy to force a refresh even if up to date).
    #[clap(long, short = 'y')]
    refresh: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Install packages as non-explicitly installed.
    #[clap(long)]
    asdeps: bool,
    /// Install pacakges as explicitly installed.
    #[clap(long)]
    asexplicit: bool,
    /// Add a virtual package to satisfy dependencies.
    #[clap(long, value_name = "package=version")]
    assumed_installed: Option<String>,
    /// Set an alternate package cache location.
    #[clap(long, value_name = "dir")]
    cachedir: Option<String>,
    /// Colorize the output.
    #[clap(long, value_name = "when", possible_values = &["always", "never", "auto"])]
    color: Option<String>,
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    config: Option<String>,
    /// Always ask for confirmation.
    #[clap(long)]
    confirm: bool,
    /// Only modify database entries, not package files.
    #[clap(long)]
    dbonly: bool,
    /// Display Pacman debug messages.
    #[clap(long)]
    debug: bool,
    /// Use relaxed timeouts for download.
    #[clap(long)]
    disable_download_timeout: bool,
    /// Set an alternate home directory for GnuPG.
    #[clap(long, value_name = "path")]
    gpgdir: Option<String>,
    /// Set an alternate hook location.
    #[clap(long, value_name = "dir")]
    hookdir: Option<String>,
    /// Ignore a package upgrade (can be used more than once).
    #[clap(long, value_name = "pkg")]
    ignore: Option<String>,
    /// Ignore a group ugrade (can be used more than once).
    #[clap(long, value_name = "grp")]
    ignoregroup: Option<String>,
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<String>,
    /// Do not reinstall up to date packages.
    #[clap(long)]
    needed: bool,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Do not show a progress bar when downloading files.
    #[clap(long)]
    noprogressbar: bool,
    /// Do not execute the install scriptlet if one exists.
    #[clap(long)]
    noscriptlet: bool,
    /// Overwrite conflicting files (can be used more than once.)
    #[clap(long, value_name = "path")]
    overwrite: Option<String>,
    /// Specify how the targets should be printed.
    #[clap(long, value_name = "string")]
    print_format: Option<String>,
    /// Operate on a mounted guests system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to search/install.
    packages: Vec<String>,
}

#[derive(Clap, Debug)]
pub struct Misc {
    /// More verbose output.
    #[clap(long, short)]
    verbose: bool,
}
