//! Types and utilities for parsing flags from the command line.

use crate::Date;
use clap::ArgAction;
use clap::Parser;
use clap::Subcommand;
use simplelog::LevelFilter;
use std::ops::Not;
use std::path::PathBuf;
use unic_langid::LanguageIdentifier;

/// Global options only applicable to Aura that must be removed from the
/// top-level args list before sending it to Pacman.
pub const AURA_GLOBALS: &[&str] = &[
    "--english",
    "--japanese",
    "--日本語",
    "--german",
    "--deutsch",
    "--polish",
    "--polski",
    "--croatian",
    "--hrvatski",
    "--swedish",
    "--svenska",
    "--spanish",
    "--español",
    "--portuguese",
    "--português",
    "--french",
    "--français",
    "--russian",
    "--русский",
    "--italian",
    "--italiano",
    "--serbian",
    "--cрпски",
    "--norwegian",
    "--norsk",
    "--indonesian",
    "--simplified-chinese",
    "--简体字",
    "--esperanto",
    "--dutch",
    "--nederlands",
    "--turkish",
    "--ukrainian",
    "--українська",
    "--romanian",
    "--română",
    "--czech",
    "--český",
    "--korean",
    "--한국어",
    "--hindi",
    "--हिंदी",
];

/// Commandline arguments to the Aura executable.
#[derive(Parser, Debug)]
#[clap(version, author, about)]
#[clap(long_about = "
Aura is a system management tool for Arch Linux and its derivatives.

This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions; see 'aura open --license' for details.
")]
#[clap(propagate_version = true, disable_help_subcommand = true)]
pub struct Args {
    // --- Aura Language Options --- //
    /// Output in English.
    #[clap(
        group = "language",
        long,
        global = true,
        hide_short_help = true,
        display_order = 10
    )]
    pub english: bool,
    /// Output in Japanese (alias: 日本語).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "日本語",
        hide_short_help = true,
        display_order = 10
    )]
    pub japanese: bool,
    /// Output in German (alias: deutsch).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "deutsch",
        hide_short_help = true,
        display_order = 10
    )]
    pub german: bool,
    /// Output in Polish (alias: polski).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "polski",
        hide_short_help = true,
        display_order = 10
    )]
    pub polish: bool,
    /// Output in Croatian (alias: hrvatski).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "hrvatski",
        hide_short_help = true,
        display_order = 10
    )]
    pub croatian: bool,
    /// Output in Swedish (alias: svenska).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "svenska",
        hide_short_help = true,
        display_order = 10
    )]
    pub swedish: bool,
    /// Output in Spanish (alias: español).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "español",
        hide_short_help = true,
        display_order = 10
    )]
    pub spanish: bool,
    /// Output in Portuguese (alias: português).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "português",
        hide_short_help = true,
        display_order = 10
    )]
    pub portuguese: bool,
    /// Output in French (alias: français).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "français",
        hide_short_help = true,
        display_order = 10
    )]
    pub french: bool,
    /// Output in Russian (alias: русский).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "русский",
        hide_short_help = true,
        display_order = 10
    )]
    pub russian: bool,
    /// Output in Italian (alias: italiano).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "italiano",
        hide_short_help = true,
        display_order = 10
    )]
    pub italian: bool,
    /// Output in Serbian (alias: cрпски).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "cрпски",
        hide_short_help = true,
        display_order = 10
    )]
    pub serbian: bool,
    /// Output in Norwegian (alias: norsk).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "norsk",
        hide_short_help = true,
        display_order = 10
    )]
    pub norwegian: bool,
    /// Output in Indonesian.
    #[clap(
        group = "language",
        long,
        global = true,
        hide_short_help = true,
        display_order = 10
    )]
    pub indonesian: bool,
    /// Output in Simplified Chinese (alias: 简体字).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "简体字",
        hide_short_help = true,
        display_order = 10
    )]
    pub simplified_chinese: bool,
    /// Output in Esperanto.
    #[clap(
        group = "language",
        long,
        global = true,
        hide_short_help = true,
        display_order = 10
    )]
    pub esperanto: bool,
    /// Output in Dutch (alias: nederlands).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "nederlands",
        hide_short_help = true,
        display_order = 10
    )]
    pub dutch: bool,
    /// Output in Turkish.
    #[clap(
        group = "language",
        long,
        global = true,
        hide_short_help = true,
        display_order = 10
    )]
    pub turkish: bool,
    /// Output in Arabic (alias: العربية).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "العربية",
        hide_short_help = true,
        display_order = 10
    )]
    pub arabic: bool,
    /// Output in Ukrainian (alias: українська).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "українська",
        hide_short_help = true,
        display_order = 10
    )]
    pub ukrainian: bool,
    /// Output in Romanian (alias: română).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "română",
        hide_short_help = true,
        display_order = 10
    )]
    pub romanian: bool,
    /// Output in Vietnamese.
    #[clap(
        group = "language",
        long,
        global = true,
        hide_short_help = true,
        display_order = 10
    )]
    pub vietnamese: bool,
    /// Output in Czech (alias: český).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "český",
        hide_short_help = true,
        display_order = 10
    )]
    pub czech: bool,
    /// Output in Korean (alias: 한국어).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "한국어",
        hide_short_help = true,
        display_order = 10
    )]
    pub korean: bool,
    /// Output in Hindi (alias: हिंदी).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "हिंदी",
        hide_short_help = true,
        display_order = 10
    )]
    pub hindi: bool,

    // --- Other Aura Options --- //
    /// Minimum level of Aura log messages to display.
    #[clap(long, value_name = "level", global = true, display_order = 9)]
    pub log_level: Option<LevelFilter>,
    /// The Pacman/Aura subcommand to run.
    #[clap(subcommand)]
    pub subcmd: SubCmd,
}

impl Args {
    /// If a language flag was given on the command line, extract the
    /// corresponding standardized language code.
    pub fn language(&self) -> Option<LanguageIdentifier> {
        match () {
            _ if self.english => Some(crate::ENGLISH),
            _ if self.japanese => Some(crate::JAPANESE),
            _ if self.polish => Some(crate::POLISH),
            _ if self.croatian => Some(crate::CROATIAN),
            _ if self.swedish => Some(crate::SWEDISH),
            _ if self.german => Some(crate::GERMAN),
            _ if self.spanish => Some(crate::SPANISH),
            _ if self.portuguese => Some(crate::PORTUGUESE),
            _ if self.french => Some(crate::FRENCH),
            _ if self.russian => Some(crate::RUSSIAN),
            _ if self.italian => Some(crate::ITALIAN),
            _ if self.serbian => Some(crate::SERBIAN),
            _ if self.norwegian => Some(crate::NORWEGIAN),
            _ if self.indonesian => Some(crate::INDONESIAN),
            _ if self.simplified_chinese => Some(crate::SIMPLIFIED_CHINESE),
            _ if self.esperanto => Some(crate::ESPERANTO),
            _ if self.dutch => Some(crate::DUTCH),
            _ if self.turkish => Some(crate::TURKISH),
            _ if self.arabic => Some(crate::ARABIC),
            _ if self.ukrainian => Some(crate::UKRAINIAN),
            _ if self.romanian => Some(crate::ROMANIAN),
            _ if self.vietnamese => Some(crate::VIETNAMESE),
            _ if self.czech => Some(crate::CZECH),
            _ if self.korean => Some(crate::KOREAN),
            _ if self.hindi => Some(crate::HINDI),
            _ => None,
        }
    }
}

/// The Aura Package Manager.
#[derive(Subcommand, Debug)]
pub enum SubCmd {
    // --- Pacman Commands --- //
    /// Operate on the package database.
    #[clap(display_order = 1)]
    Database(Database),
    /// Query the files database.
    #[clap(display_order = 1)]
    Files(Files),
    /// Query the package database.
    #[clap(display_order = 1)]
    Query(Query),
    /// Remove packages from the system.
    #[clap(display_order = 1)]
    Remove(Remove),
    /// Synchronize official packages.
    #[clap(display_order = 1)]
    Sync(Sync),
    /// Check if given dependencies are satisfied.
    #[clap(display_order = 1)]
    DepTest(DepTest),
    /// Upgrade or add packages to the system.
    #[clap(display_order = 1)]
    Upgrade(Upgrade),
    // --- Aura Commands --- //
    /// Synchronize AUR packages.
    #[clap(display_order = 1)]
    Aur(Aur),
    /// Save and restore the global package state.
    #[clap(display_order = 1)]
    Backup(Backup),
    /// Manage the package cache.
    #[clap(display_order = 1)]
    Cache(Cache),
    /// View the Pacman/ALPM log.
    #[clap(display_order = 1)]
    Log(Log),
    /// Handle orphan packages.
    #[clap(display_order = 1)]
    Orphans(Orphans),
    /// Validate your system.
    Check(Check),
    /// View various configuration settings and files.
    Conf(Conf),
    /// Output a dependency graph in DOT format.
    Deps(Deps),
    /// State of Free Software installed on the system.
    Free(Free),
    /// View statistics about your machine or about Aura itself.
    Stats(Stats),
    /// The people behind Aura.
    Thanks,
}

/// Synchronize official packages.
#[derive(Parser, Debug)]
#[clap(short_flag = 'S', long_flag = "sync")]
pub struct Sync {
    /// Remove old packages from cache directory (-cc for all).
    #[clap(
        group = "sync",
        long,
        short,
        action(ArgAction::Count),
        display_order = 1
    )]
    clean: u8,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', action(ArgAction::Count), display_order = 2)]
    nodeps: u8,
    /// View all members of a package group (-gg to view all groups and members).
    #[clap(
        group = "sync",
        long,
        short,
        action(ArgAction::Count),
        display_order = 1
    )]
    groups: u8,
    /// View package information.
    #[clap(
        group = "sync",
        long,
        short,
        value_name = "packages",
        num_args = 1..,
        display_order = 1
    )]
    info: Vec<String>,
    /// View a list of packages in a repo.
    #[clap(group = "sync", long, short, value_name = "repo", display_order = 1)]
    list: Option<String>,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 2)]
    print: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 2)]
    quiet: bool,
    /// Search remote repositories for matching strings.
    #[clap(group = "sync", long, short, display_order = 1)]
    search: bool,
    /// Upgrade installed packages (-uu enables downgrades).
    #[clap(
        group = "sync",
        long,
        short = 'u',
        action(ArgAction::Count),
        display_order = 1
    )]
    sysupgrade: u8,
    /// Be verbose.
    #[clap(long, short, display_order = 2)]
    verbose: bool,
    /// Download packages but do not install/upgrade anything.
    #[clap(
        group = "sync",
        long,
        short = 'w',
        value_name = "packages",
        num_args = 1..,
        display_order = 1
    )]
    downloadonly: Vec<String>,
    /// Download fresh package databases from the server (-yy to force a refresh even if up to date).
    #[clap(long, short = 'y', action(ArgAction::Count), display_order = 1)]
    refresh: u8,
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
    #[clap(long, value_name = "path")]
    cachedir: Option<PathBuf>,
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
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
    logfile: Option<PathBuf>,
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
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to search/install.
    packages: Vec<String>,
}

impl Sync {
    /// Does this `-S` subflag need sudo?
    pub fn needs_sudo(&self) -> bool {
        (self.info.is_empty().not() || self.search || self.list.is_some() || self.print).not()
    }
}

// TODO Reconcile `pacman -Th` and the manpage entry for -T.
// TODO Is it possible to disable subcommand "plan names"? i.e. to have only
// their long/short variants remain valid (or at least shown in `-h`).
/// Check if given dependencies are satisfied.
#[derive(Parser, Debug)]
#[clap(short_flag = 'T', long_flag = "deptest")]
pub struct DepTest {
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
    color: Option<String>,
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    config: Option<String>,
    /// Always ask for confirmation.
    #[clap(long)]
    confirm: bool,
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
    /// Display Pacman debug messages.
    #[clap(long)]
    debug: bool,
    // TODO This shouldn't make any sense for -T. Why does it appear in -Th? Is
    // it actually used?
    /// Use relaxed timeouts for download.
    #[clap(long)]
    disable_download_timeout: bool,
    /// Set an alternate home directory for GnuPG.
    #[clap(long, value_name = "path")]
    gpgdir: Option<String>,
    /// Set an alternate hook location.
    #[clap(long, value_name = "dir")]
    hookdir: Option<String>,
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<PathBuf>,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Dependencies to check.
    packages: Vec<String>,
}

/// Upgrade or add packages to the system.
#[derive(Parser, Debug)]
#[clap(short_flag = 'U', long_flag = "upgrade")]
pub struct Upgrade {
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', display_order = 1)]
    nodeps: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 1)]
    print: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
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
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
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
    logfile: Option<PathBuf>,
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
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to install, either a tarball or a URL.
    packages: Vec<String>,
}

impl Upgrade {
    /// Does this `-U` subflag need sudo?
    pub fn needs_sudo(&self) -> bool {
        self.print.not()
    }
}

// TODO `pacman -Fh` does not include the top-level usage line!
/// Query the files database.
#[derive(Parser, Debug)]
#[clap(short_flag = 'F', long_flag = "files")]
pub struct Files {
    /// View a list of files belonging to a package.
    #[clap(long, short, display_order = 1)]
    list: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Enable searching using regular expressions.
    #[clap(long, short = 'x', display_order = 1)]
    regex: bool,
    /// Download fresh package databases from the server (-yy to force a refresh even if up to date).
    #[clap(long, short = 'y', action(ArgAction::Count), display_order = 1)]
    refresh: u8,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
    color: Option<String>,
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    config: Option<String>,
    /// Always ask for confirmation.
    #[clap(long)]
    confirm: bool,
    /// Display Pacman debug messages.
    #[clap(long)]
    debug: bool,
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
    // TODO Here too.
    /// Use relaxed timeouts for download.
    #[clap(long)]
    disable_download_timeout: bool,
    /// Set an alternate home directory for GnuPG.
    #[clap(long, value_name = "path")]
    gpgdir: Option<String>,
    /// Set an alternate hook location.
    #[clap(long, value_name = "dir")]
    hookdir: Option<String>,
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<PathBuf>,
    /// Produce machine-readable output.
    #[clap(long)]
    machinereadable: bool,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Files to search.
    files: Vec<String>,
}

impl Files {
    pub fn needs_sudo(&self) -> bool {
        self.refresh > 0
    }
}

/// Remove packages from the system.
#[derive(Parser, Debug)]
#[clap(short_flag = 'R', long_flag = "remove")]
pub struct Remove {
    /// Remove packages and all packages that depend on them.
    #[clap(long, short, display_order = 1)]
    cascade: bool,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', action = ArgAction::Count, display_order = 1)]
    nodeps: u8,
    /// Remove configuration files.
    #[clap(long, short = 'n', display_order = 1)]
    nosave: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 1)]
    print: bool,
    /// Remove unnecessary dependencies (-ss includes explicitly installed dependencies).
    #[clap(long, short = 's', action = ArgAction::Count, display_order = 1)]
    recursive: u8,
    /// Remove unneeded packages.
    #[clap(long, short, display_order = 1)]
    unneeded: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Add a virtual package to satisfy dependencies.
    #[clap(long, value_name = "package=version")]
    assumed_installed: Option<String>,
    /// Set an alternate package cache location.
    #[clap(long, value_name = "path")]
    cachedir: Option<PathBuf>,
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
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
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<PathBuf>,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Do not show a progress bar when downloading files.
    #[clap(long)]
    noprogressbar: bool,
    /// Do not execute the install scriptlet if one exists.
    #[clap(long)]
    noscriptlet: bool,
    /// Specify how the targets should be printed.
    #[clap(long, value_name = "string")]
    print_format: Option<String>,
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to remove.
    packages: Vec<String>,
}

impl Remove {
    /// Does this `-R` subflag need sudo?
    pub fn needs_sudo(&self) -> bool {
        self.print.not()
    }
}

/// Operate on the package database.
#[derive(Parser, Debug)]
#[clap(short_flag = 'D', long_flag = "database")]
pub struct Database {
    /// Test local database for validity (-kk for sync databases).
    #[clap(long, short = 'k', action(ArgAction::Count), display_order = 1)]
    check: u8,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 2)]
    quiet: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 2)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Mark packages as non-explicitly installed.
    #[clap(long, display_order = 1)]
    asdeps: bool,
    /// Mark pacakges as explicitly installed.
    #[clap(long, display_order = 1)]
    asexplicit: bool,
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
    color: Option<String>,
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    config: Option<String>,
    /// Always ask for confirmation.
    #[clap(long)]
    confirm: bool,
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
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
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<PathBuf>,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to modify.
    packages: Vec<String>,
}

impl Database {
    pub fn needs_sudo(&self) -> bool {
        self.asdeps || self.asexplicit
    }
}

/// Query the package database.
#[derive(Parser, Debug)]
#[clap(short_flag = 'Q', long_flag = "query")]
pub struct Query {
    /// View the changelog of a package.
    #[clap(long, short, display_order = 1)]
    changelog: bool,
    /// List packages installed as dependencies [filter].
    #[clap(long, short, display_order = 1)]
    deps: bool,
    /// List packages explicitly installed [filter].
    #[clap(long, short, display_order = 1)]
    explicit: bool,
    /// View all members of a package group.
    #[clap(long, short, display_order = 1)]
    groups: bool,
    /// View package information (-ii for backup files).
    #[clap(long, short, display_order = 1)]
    info: bool,
    /// Check that package files exist (-kk for file properties).
    #[clap(long, short = 'k', action(ArgAction::Count), display_order = 1)]
    check: u8,
    /// List the files owned by the queried package.
    #[clap(long, short, display_order = 1)]
    list: bool,
    /// List installed packages not found in sync db(s) [filter].
    #[clap(long, short = 'm', display_order = 1)]
    foreign: bool,
    /// List installed packages only found in sync db(s) [filter].
    #[clap(long, short, display_order = 1)]
    /// Print the targets instead of performing the operation.
    native: bool,
    /// Query the package that owns <file>.
    #[clap(long, short, value_name = "file", display_order = 1)]
    owns: Option<String>,
    /// Query a package file instead of the database.
    #[clap(long, short = 'p', value_name = "package", display_order = 1)]
    file: Option<String>,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
    /// Search remote repositories for matchings strings.
    #[clap(long, short, display_order = 1)]
    search: bool,
    /// List packages not (optionally) required by any package (-tt to ignore optdepends) [filter].
    #[clap(long, short = 't', display_order = 1)]
    unrequired: bool,
    /// List outdated packages [filter].
    #[clap(long, short, display_order = 1)]
    upgrades: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Set an alternate package cache location.
    #[clap(long, value_name = "path")]
    cachedir: Option<PathBuf>,
    /// Colorize the output.
    #[clap(long, value_name = "when", value_parser = ["always", "never", "auto"])]
    color: Option<String>,
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    config: Option<String>,
    /// Always ask for confirmation.
    #[clap(long)]
    confirm: bool,
    /// Display Pacman debug messages.
    #[clap(long)]
    debug: bool,
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path")]
    dbpath: Option<String>,
    /// Use relaxed timeouts for download.
    #[clap(long)]
    disable_download_timeout: bool,
    /// Set an alternate home directory for GnuPG.
    #[clap(long, value_name = "path")]
    gpgdir: Option<String>,
    /// Set an alternate hook location.
    #[clap(long, value_name = "dir")]
    hookdir: Option<String>,
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<PathBuf>,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Set an alternate installation root.
    #[clap(long, value_name = "path")]
    root: Option<String>,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to query.
    packages: Vec<String>,
}

/// Perform security analysis of a PKGBUILD.
#[derive(Parser, Debug)]
#[clap(short_flag = 'P', long_flag = "analysis")]
pub struct Analysis {
    /// Analyse a given PKGBUILD.
    #[clap(group = "analyse", long, short, value_name = "path")]
    pub file: Option<String>,
    /// Analyse a PKGBUILD found in the specified directory.
    #[clap(group = "analyse", long, short, value_name = "path")]
    pub dir: Option<String>,
    /// Analyse the PKGBUILDs of all locally installed AUR packages.
    #[clap(group = "analyse", long, short)]
    pub audit: bool,
}

/// Handle orphan packages.
#[derive(Parser, Debug)]
#[clap(short_flag = 'O', long_flag = "orphans")]
pub struct Orphans {
    /// Mark a package as being explicitly installed.
    #[clap(group = "orphans", long, short, value_name = "packages", num_args = 1..)]
    pub adopt: Vec<String>,
    /// Uninstall all orphan packages.
    #[clap(group = "orphans", long, short = 'j')]
    pub abandon: bool,
    /// Display all explicitly installed, top-level packages.
    #[clap(group = "orphans", long, short = 'e')]
    pub elderly: bool,
}

/// View various configuration settings and files.
#[derive(Parser, Debug)]
pub struct Conf {
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    pub config: Option<PathBuf>,
    /// View the Pacman conf.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub pacman: bool,
    /// View the contents of ~/.config/aura/config.toml.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub aura: bool,
    /// View the Makepkg conf.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub makepkg: bool,
    /// Output your current, full Aura config as legal TOML.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub gen: bool,
}

#[derive(Parser, Debug)]
#[clap(short_flag = 'L', long_flag = "viewlog")]
/// View the Pacman/ALPM log.
pub struct Log {
    /// Display install/upgrade history for the given packages.
    #[clap(group = "log", long, short, value_name = "packages", num_args = 1.., display_order = 1)]
    pub info: Vec<String>,

    /// Search the Pacman log for a matching string.
    #[clap(group = "log", long, short, value_name = "term", display_order = 1)]
    pub search: Option<String>,

    /// Only display log entries from before the given date.
    #[clap(long, short, value_name = "YYYY-MM-DD")]
    pub before: Option<Date>,

    /// Only display log entries from after the given date.
    #[clap(long, short, value_name = "YYYY-MM-DD")]
    pub after: Option<Date>,

    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<PathBuf>,
}

/// View statistics about your machine or Aura itself.
#[derive(Parser, Debug)]
pub struct Stats {
    /// View Aura's localization statistics.
    #[clap(group = "stats", long, short, display_order = 1)]
    pub lang: bool,

    /// View all installed package groups.
    #[clap(group = "stats", long, short, display_order = 1)]
    pub groups: bool,

    /// View the Top 10 heaviest installed packages.
    #[clap(group = "stats", long, display_order = 1)]
    pub heavy: bool,
}

/// State of Free Software installed on the system.
#[derive(Parser, Debug)]
pub struct Free {
    /// Consider only Copyleft licenses.
    #[clap(long, display_order = 1)]
    pub copyleft: bool,
    /// Allow FOSS-derived custom licenses.
    #[clap(long, display_order = 2)]
    pub lenient: bool,
}

/// Synchronize AUR packages.
#[derive(Parser, Debug)]
#[clap(short_flag = 'A', long_flag = "aursync")]
pub struct Aur {
    /// View AUR package information.
    #[clap(
        group = "aur",
        long,
        short,
        value_name = "packages",
        num_args = 1..,
        display_order = 1
    )]
    pub info: Vec<String>,

    /// Search the AUR via search strings. Multiple terms narrow the result.
    #[clap(
        group = "aur",
        long,
        short,
        value_name = "terms",
        num_args = 1..,
        display_order = 1
    )]
    pub search: Vec<String>,

    /// Search the AUR for packages that "provide" some package identity.
    #[clap(
        group = "aur",
        long,
        short = 'v',
        value_name = "package",
        display_order = 1
    )]
    pub provides: Option<String>,

    /// [-s/-v] Sort results alphabetically.
    #[clap(long, display_order = 2)]
    pub abc: bool,

    /// [-s/-v] Limit the results to N results.
    #[clap(long, value_name = "N", display_order = 2, alias = "head")]
    pub limit: Option<usize>,

    /// [-s/-v] Reverse the search results.
    #[clap(long, short, display_order = 2)]
    pub reverse: bool,

    /// [-s/-v] Only print matching package names.
    #[clap(long, short, display_order = 2)]
    pub quiet: bool,

    /// Open a given package's AUR package.
    #[clap(group = "aur", long, short, value_name = "package", display_order = 1)]
    pub open: Option<String>,

    /// View a package's PKGBUILD.
    #[clap(group = "aur", long, short, value_name = "package", display_order = 1)]
    pub pkgbuild: Option<String>,

    /// The path in which to build packages.
    #[clap(long, display_order = 4, value_name = "path")]
    pub build: Option<PathBuf>,

    /// The user to build as.
    #[clap(long, display_order = 4, value_name = "user")]
    pub builduser: Option<String>,

    /// View diffs of PKGBUILDs and related build files before building.
    #[clap(long, short = 'k', display_order = 4)]
    pub diff: bool,

    /// View/edit PKGBUILDs and related build files before building.
    #[clap(long, display_order = 4)]
    pub hotedit: bool,

    /// Run shellcheck on PKGBUILDs before building.
    #[clap(long, display_order = 4)]
    pub shellcheck: bool,

    /// Remove makedeps after building.
    #[clap(long, short = 'a', display_order = 4)]
    pub delmakedeps: bool,

    /// Upgrade all installed AUR packages.
    #[clap(group = "aur", long, short = 'u', display_order = 1)]
    pub sysupgrade: bool,

    /// [-u] Rebuild all git/svn/hg/etc. packages as well.
    #[clap(long, display_order = 3, alias = "devel")]
    pub git: bool,

    /// [-u] Ignore a package upgrade (can be used more than once).
    #[clap(
        long,
        value_name = "package",
        action(ArgAction::Append),
        display_order = 3
    )]
    pub ignore: Vec<String>,

    /// [-u] Show available upgrades, but do not perform them.
    #[clap(long, short = 'd', display_order = 3)]
    pub dryrun: bool,

    /// Clone a package's AUR repository, but don't build anything.
    #[clap(
        group = "aur",
        long = "clone",
        short = 'w',
        value_name = "package",
        action(ArgAction::Append),
        display_order = 1
    )]
    pub wclone: Vec<String>,

    /// Deprecated.
    #[clap(long, short = 'x', display_order = 1)]
    #[deprecated(since = "4.0.0", note = "Makepkg output is now shown by default.")]
    pub unsuppress: bool,

    /// Pull the latest changes for every local copy of an AUR package.
    #[clap(long, short = 'y', display_order = 1)]
    pub refresh: bool,

    /// Do not ask for any confirmation.
    #[clap(long, display_order = 5)]
    pub noconfirm: bool,

    /// Do not consider checkdeps when building packages.
    #[clap(long, display_order = 5)]
    pub nocheck: bool,

    /// Perform no dependency resolution.
    #[clap(long, display_order = 5)]
    pub skipdepcheck: bool,

    /// Packages to install.
    pub packages: Vec<String>,
}

/// Save and restore the global package state.
#[derive(Parser, Debug)]
#[clap(short_flag = 'B', long_flag = "backup")]
pub struct Backup {
    /// Show all saved package snapshot filenames.
    #[clap(group = "backup", long, short, display_order = 1)]
    pub list: bool,

    /// Remove all snapshots without matching tarballs in the cache.
    #[clap(group = "backup", long, short, display_order = 1)]
    pub clean: bool,

    /// Restore to a previous package snapshot.
    #[clap(group = "backup", long, short, display_order = 1)]
    pub restore: bool,
}

/// Manage the package cache.
#[derive(Parser, Debug)]
#[clap(short_flag = 'C', long_flag = "cache")]
pub struct Cache {
    /// Search the package cache.
    #[clap(group = "cache", short, long, value_name = "term", display_order = 1)]
    pub search: Option<String>,

    // TODO Make other options elsewhere that expect a path have `PathBuf` too.
    /// Back up the package cache to a given directory.
    #[clap(group = "cache", long, short, value_name = "target", display_order = 1)]
    pub backup: Option<PathBuf>,

    /// Save the most recent <N> versions of a package.
    #[clap(group = "cache", short, long, value_name = "N", display_order = 1)]
    pub clean: Option<usize>,

    /// Delete only those tarballs which aren't present in a snapshot.
    #[clap(group = "cache", long = "notsaved", display_order = 1)]
    pub clean_unsaved: bool,

    /// Look up specific packages for info on their cache entries.
    #[clap(group = "cache", short, long, value_name = "pkg(s)", num_args = 1.., display_order = 1)]
    pub info: Vec<String>,

    /// Print the contents of the package cache.
    #[clap(group = "cache", short, long, display_order = 1)]
    pub list: bool,

    /// Download tarballs of installed packages that are missing from the cache.
    #[clap(group = "cache", short = 'y', long, display_order = 1)]
    pub refresh: bool,

    /// Delete invalid tarballs from the cache.
    #[clap(group = "cache", short = 't', long, display_order = 1)]
    pub invalid: bool,

    /// Display packages that don't have a tarball in the cache.
    #[clap(group = "cache", long, short, display_order = 1)]
    pub missing: bool,

    /// Packages to downgrade.
    pub packages: Vec<String>,
}

/// Output a dependency graph PNG.
#[derive(Parser, Debug)]
pub struct Deps {
    /// Display packages that depend on the given args.
    #[clap(long, short, display_order = 1)]
    pub reverse: bool,

    /// Include optional dependencies.
    #[clap(long, short, display_order = 1)]
    pub optional: bool,

    /// The number of layers up or down to allow.
    #[clap(long, short, value_name = "n", display_order = 1)]
    pub limit: Option<u8>,

    /// Print the raw DOT output.
    #[clap(long, display_order = 2)]
    pub raw: bool,

    /// Open the output image automatically.
    #[clap(long, conflicts_with = "raw", display_order = 2)]
    pub open: bool,

    /// Packages to focus on.
    pub packages: Vec<String>,
}

/// Validate your system.
#[derive(Parser, Debug)]
pub struct Check {}
