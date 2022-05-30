//! Types and utilities for parsing flags from the command line.

use chrono::NaiveDate;
use clap::{Parser, Subcommand};
use simplelog::LevelFilter;
use std::path::PathBuf;
use unic_langid::{langid, LanguageIdentifier};

/// Global options only applicable to Aura that must be removed from the
/// top-level args list before sending it to Pacman.
pub(crate) const AURA_GLOBALS: &[&str] = &["--english", "--japanese", "--german"];

/// Commandline arguments to the Aura executable.
#[derive(Parser, Debug)]
#[clap(version, author, about)]
#[clap(propagate_version = true, disable_help_subcommand = true)]
pub(crate) struct Args {
    // --- Global Pacman Options --- //
    /// Set an alternate database location.
    #[clap(
        long,
        // short = 'b',
        value_name = "path",
        global = true,
        display_order = 9
    )]
    pub(crate) dbpath: Option<String>,
    /// Set an alternate installation root.
    #[clap(long, value_name = "path", global = true, display_order = 9)]
    pub(crate) root: Option<String>,
    /// Set an alternate log file.
    #[clap(long, value_name = "path", global = true, display_order = 9)]
    pub(crate) logfile: Option<PathBuf>,
    /// Set an alternate package cache location.
    #[clap(long, value_name = "path", global = true, display_order = 9)]
    pub(crate) cachedir: Option<PathBuf>,

    // --- Aura Language Options --- //
    /// Output in English.
    #[clap(group = "language", long, global = true, display_order = 10)]
    pub(crate) english: bool,
    /// Output in Japanese (alias: 日本語).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "日本語",
        display_order = 10
    )]
    pub(crate) japanese: bool,
    /// Output in German (alias: deutsch).
    #[clap(
        group = "language",
        long,
        global = true,
        alias = "deutsch",
        display_order = 10
    )]
    pub(crate) german: bool,

    // --- Other Aura Options --- //
    /// Minimum level of Aura log messages to display.
    #[clap(long, value_name = "level", possible_values = &["debug", "info", "warn", "error"], global = true)]
    pub(crate) log_level: Option<LevelFilter>,
    /// The Pacman/Aura subcommand to run.
    #[clap(subcommand)]
    pub(crate) subcmd: SubCmd,
}

impl Args {
    /// If a language flag was given on the command line, extract the
    /// corresponding standardized language code.
    pub(crate) fn language(&self) -> Option<LanguageIdentifier> {
        match () {
            _ if self.english => Some(langid!("en-US")),
            _ if self.german => Some(langid!("de-DE")),
            _ if self.japanese => Some(langid!("ja-JP")),
            _ => None,
        }
    }
}

/// The Aura Package Manager.
#[derive(Subcommand, Debug)]
pub(crate) enum SubCmd {
    // --- Pacman Commands --- //
    /// Operate on the package database.
    Database(Database),
    /// Query the files database.
    Files(Files),
    /// Query the package database.
    Query(Query),
    /// Remove packages from the system.
    Remove(Remove),
    /// Synchronize official packages.
    Sync(Sync),
    /// Check if given dependencies are satisfied.
    DepTest(DepTest),
    /// Upgrade or add packages to the system.
    Upgrade(Upgrade),
    // --- Aura Commands --- //
    /// Synchronize AUR packages.
    Aur(Aur),
    /// Save and restore the global package state.
    Backup(Backup),
    /// Manage the package cache.
    Cache(Cache),
    /// View the Pacman/ALPM log.
    Log(Log),
    /// Handle orphan packages.
    Orphans(Orphans),
    /// View various configuration settings and files.
    Conf(Conf),
    /// View statistics about your machine or about Aura itself.
    Stats(Stats),
    /// Open various webpages related to Aura.
    Open(Open),
    /// Output a dependency graph in DOT format.
    Deps(Deps),
    /// Validate your system.
    Check(Check),
}

/// Synchronize official packages.
#[derive(Parser, Debug)]
#[clap(short_flag = 'S', long_flag = "sync")]
pub(crate) struct Sync {
    /// Remove old packages from cache directory (-cc for all).
    #[clap(long, short, parse(from_occurrences), display_order = 1)]
    pub(crate) clean: u8,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', parse(from_occurrences), display_order = 1)]
    nodeps: u8,
    /// View all members of a package group (-gg to view all groups and members).
    #[clap(long, short, parse(from_occurrences), display_order = 1)]
    groups: u8,
    /// View package information (-ii for extended information).
    #[clap(long, short, parse(from_occurrences), display_order = 1)]
    info: u8,
    /// View a list of packages in a repo.
    #[clap(long, short, display_order = 1)]
    list: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 1)]
    print: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
    /// Search remote repositories for matchings strings.
    #[clap(long, short, display_order = 1)]
    search: bool,
    /// Upgrade installed packages (-uu enables downgrades).
    #[clap(long, short = 'u', parse(from_occurrences), display_order = 1)]
    sysupgrade: u8,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Download packages but do not install/upgrade anything.
    #[clap(long, short = 'w', display_order = 1)]
    downloadonly: bool,
    /// Download fresh package databases from the server (-yy to force a refresh even if up to date).
    #[clap(long, short = 'y', parse(from_occurrences), display_order = 1)]
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
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to search/install.
    packages: Vec<String>,
}

// TODO Reconcile `pacman -Th` and the manpage entry for -T.
// TODO Is it possible to disable subcommand "plan names"? i.e. to have only
// their long/short variants remain valid (or at least shown in `-h`).
/// Check if given dependencies are satisfied.
#[derive(Parser, Debug)]
#[clap(short_flag = 'T', long_flag = "deptest")]
pub(crate) struct DepTest {
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Colorize the output.
    #[clap(long, value_name = "when", possible_values = &["always", "never", "auto"])]
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
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Dependencies to check.
    packages: Vec<String>,
}

/// Upgrade or add packages to the system.
#[derive(Parser, Debug)]
#[clap(short_flag = 'U', long_flag = "upgrade")]
pub(crate) struct Upgrade {
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
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to install, either a tarball or a URL.
    packages: Vec<String>,
}

// TODO `pacman -Fh` does not include the top-level usage line!
/// Query the files database.
#[derive(Parser, Debug)]
#[clap(short_flag = 'F', long_flag = "files")]
pub(crate) struct Files {
    /// View a list of packages in a repo.
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
    #[clap(long, short = 'y', display_order = 1)]
    refresh: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
    /// Colorize the output.
    #[clap(long, value_name = "when", possible_values = &["always", "never", "auto"])]
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
    /// Produce machine-readable output.
    #[clap(long)]
    machinereadable: bool,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Files to search.
    files: Vec<String>,
}

/// Remove packages from the system.
#[derive(Parser, Debug)]
#[clap(short_flag = 'R', long_flag = "remove")]
pub(crate) struct Remove {
    /// Remove packages and all packages that depend on them.
    #[clap(long, short, display_order = 1)]
    cascade: bool,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', display_order = 1)]
    nodeps: bool,
    /// Remove configuration files.
    #[clap(long, short = 'n', display_order = 1)]
    nosave: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 1)]
    print: bool,
    /// Remove unnecessary dependencies (-ss includes explicitly installed dependencies).
    #[clap(long, short = 's', display_order = 1)]
    recursive: bool,
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
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to remove.
    packages: Vec<String>,
}

/// Operate on the package database.
#[derive(Parser, Debug)]
#[clap(short_flag = 'D', long_flag = "database")]
pub(crate) struct Database {
    /// Test local database for validity (-kk for sync databases).
    #[clap(long, short = 'k', multiple_occurrences = true, display_order = 1)]
    check: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
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
    /// Colorize the output.
    #[clap(long, value_name = "when", possible_values = &["always", "never", "auto"])]
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
    /// Use relaxed timeouts for download.
    #[clap(long)]
    disable_download_timeout: bool,
    /// Set an alternate home directory for GnuPG.
    #[clap(long, value_name = "path")]
    gpgdir: Option<String>,
    /// Set an alternate hook location.
    #[clap(long, value_name = "dir")]
    hookdir: Option<String>,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to modify.
    packages: Vec<String>,
}

/// Query the package database.
#[derive(Parser, Debug)]
#[clap(short_flag = 'Q', long_flag = "query")]
pub(crate) struct Query {
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
    #[clap(long, short = 'k', multiple_occurrences = true, display_order = 1)]
    check: bool,
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
    /// Colorize the output.
    #[clap(long, value_name = "when", possible_values = &["always", "never", "auto"])]
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
    /// Use relaxed timeouts for download.
    #[clap(long)]
    disable_download_timeout: bool,
    /// Set an alternate home directory for GnuPG.
    #[clap(long, value_name = "path")]
    gpgdir: Option<String>,
    /// Set an alternate hook location.
    #[clap(long, value_name = "dir")]
    hookdir: Option<String>,
    /// Do not ask for any confirmation.
    #[clap(long)]
    noconfirm: bool,
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to query.
    packages: Vec<String>,
}

/// Perform security analysis of a PKGBUILD.
#[derive(Parser, Debug)]
#[clap(short_flag = 'P', long_flag = "analysis")]
pub(crate) struct Analysis {
    /// Analyse a given PKGBUILD.
    #[clap(group = "analyse", long, short, value_name = "path")]
    pub(crate) file: Option<String>,
    /// Analyse a PKGBUILD found in the specified directory.
    #[clap(group = "analyse", long, short, value_name = "path")]
    pub(crate) dir: Option<String>,
    /// Analyse the PKGBUILDs of all locally installed AUR packages.
    #[clap(group = "analyse", long, short)]
    pub(crate) audit: bool,
}

/// Handle orphan packages.
#[derive(Parser, Debug)]
#[clap(short_flag = 'O', long_flag = "orphans")]
pub(crate) struct Orphans {
    /// Mark a package as being explicitly installed.
    #[clap(group = "orphans", long, short, value_name = "packages")]
    pub(crate) adopt: Vec<String>,
    /// Uninstall all orphan packages.
    #[clap(group = "orphans", long, short = 'j')]
    pub(crate) abandon: bool,
}

/// View various configuration settings and files.
#[derive(Parser, Debug)]
pub(crate) struct Conf {
    /// Set an alternate Pacman configuration file.
    #[clap(long, value_name = "path")]
    pub(crate) config: Option<String>,
    /// View the Pacman conf.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub(crate) pacman: bool,
    /// View the contents of ~/.config/aura.toml.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub(crate) aura: bool,
    /// View the Makepkg conf.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub(crate) makepkg: bool,
    /// List all .pacnew files newer than their originals.
    #[clap(group = "conf", long = "new", display_order = 1)]
    pub(crate) pacnew: bool,
    /// Output your current, full Aura config as legal TOML.
    #[clap(group = "conf", long, short, display_order = 1)]
    pub(crate) gen: bool,
}

#[derive(Parser, Debug)]
#[clap(short_flag = 'L', long_flag = "viewlog")]
/// View the Pacman/ALPM log.
pub(crate) struct Log {
    /// Display install/upgrade history for the given packages.
    #[clap(group = "log", long, short, value_name = "packages", display_order = 1)]
    pub(crate) info: Vec<String>,

    /// Search the Pacman log for a matching string.
    #[clap(group = "log", long, short, value_name = "term", display_order = 1)]
    pub(crate) search: Option<String>,

    /// Only display log entries from before the given date.
    #[clap(long, short, value_name = "YYYY-MM-DD")]
    pub(crate) before: Option<NaiveDate>,

    /// Only display log entries from after the given date.
    #[clap(long, short, value_name = "YYYY-MM-DD")]
    pub(crate) after: Option<NaiveDate>,
}

/// View statistics about your machine or Aura itself.
#[derive(Parser, Debug)]
pub(crate) struct Stats {
    /// View Aura's localization statistics.
    #[clap(group = "stats", long, short, display_order = 1)]
    pub(crate) lang: bool,

    /// View all installed package groups.
    #[clap(group = "stats", long, short, display_order = 1)]
    pub(crate) groups: bool,

    /// View the Top 10 heaviest installed packages.
    #[clap(group = "stats", long, display_order = 1)]
    pub(crate) heavy: bool,
}

/// Synchronize AUR packages.
#[derive(Parser, Debug)]
#[clap(short_flag = 'A', long_flag = "aursync")]
pub(crate) struct Aur {
    /// View AUR package information.
    #[clap(
        group = "aur",
        long,
        short,
        value_name = "packages",
        multiple_values = true,
        display_order = 1
    )]
    pub(crate) info: Vec<String>,

    /// Search the AUR via search strings. Multiple terms narrow the result.
    #[clap(
        group = "aur",
        long,
        short,
        value_name = "terms",
        multiple_values = true,
        display_order = 1
    )]
    pub(crate) search: Vec<String>,

    // TODO Avoid boolean blindness.
    /// [-s] Sort results alphabetically.
    #[clap(long, display_order = 2)]
    pub(crate) abc: bool,

    /// [-s] Limit the results to N results.
    #[clap(long, value_name = "N", display_order = 2)]
    pub(crate) limit: Option<usize>,

    /// [-s] Reverse the search results.
    #[clap(long, short, display_order = 2)]
    pub(crate) reverse: bool,

    /// [-s] Only print matching package names.
    #[clap(long, short, display_order = 2)]
    pub(crate) quiet: bool,

    /// Open a given package's AUR package.
    #[clap(group = "aur", long, short, value_name = "package", display_order = 1)]
    pub(crate) open: Option<String>,

    /// View a package's PKGBUILD.
    #[clap(group = "aur", long, short, value_name = "package", display_order = 1)]
    pub(crate) pkgbuild: Option<String>,

    /// Upgrade all installed AUR packages.
    #[clap(group = "aur", long, short = 'u', display_order = 1)]
    pub(crate) sysupgrade: bool,

    /// [-u] Rebuild all git/svn/hg/etc. packages as well.
    #[clap(long, display_order = 3)]
    pub(crate) git: bool,

    /// [-u] Ignore a package upgrade (can be used more than once).
    #[clap(
        long,
        value_name = "package",
        multiple_occurrences = true,
        display_order = 3
    )]
    pub(crate) ignore: Vec<String>,

    /// Clone a package's AUR repository, but don't build anything.
    #[clap(
        group = "aur",
        long = "clone",
        short = 'w',
        value_name = "package",
        multiple_values = true,
        display_order = 1
    )]
    pub(crate) wclone: Vec<String>,

    /// Pull the latest changes for every local copy of an AUR package.
    #[clap(group = "aur", long, short = 'y', display_order = 1)]
    pub(crate) refresh: bool,

    /// Packages to install.
    pub(crate) packages: Vec<String>,
}

/// Save and restore the global package state.
#[derive(Parser, Debug)]
#[clap(short_flag = 'B', long_flag = "backup")]
pub(crate) struct Backup {
    /// Show all saved package snapshot filenames.
    #[clap(group = "backup", long, short, display_order = 1)]
    pub(crate) list: bool,

    /// Remove all snapshots without matching tarballs in the cache.
    #[clap(group = "backup", long, short, display_order = 1)]
    pub(crate) clean: bool,

    /// Restore to a previous package snapshot.
    #[clap(group = "backup", long, short, display_order = 1)]
    pub(crate) restore: bool,
}

/// Manage the package cache.
#[derive(Parser, Debug)]
#[clap(short_flag = 'C', long_flag = "cache")]
pub(crate) struct Cache {
    /// Search the package cache.
    #[clap(group = "cache", short, long, value_name = "term", display_order = 1)]
    pub(crate) search: Option<String>,

    // TODO Make other options elsewhere that expect a path have `PathBuf` too.
    // TODO Restore the `short` flag for this option after resolving the conflict with `--dbpath`!
    /// Back up the package cache to a given directory.
    #[clap(group = "cache", long, short, value_name = "target", display_order = 1)]
    pub(crate) backup: Option<PathBuf>,

    /// Save the most recent <N> versions of a package.
    #[clap(group = "cache", short, long, value_name = "N", display_order = 1)]
    pub(crate) clean: Option<usize>,

    /// Delete only those tarballs which aren't present in a snapshot.
    #[clap(group = "cache", long = "notsaved", display_order = 1)]
    pub(crate) clean_unsaved: bool,

    /// Look up specific packages for info on their cache entries.
    #[clap(group = "cache", short, long, value_name = "pkg(s)", display_order = 1)]
    pub(crate) info: Vec<String>,

    /// Print the contents of the package cache.
    #[clap(group = "cache", short, long, display_order = 1)]
    pub(crate) list: bool,

    /// Download tarballs of installed packages that are missing from the cache.
    #[clap(group = "cache", short = 'y', long, display_order = 1)]
    pub(crate) refresh: bool,

    /// Delete invalid tarballs from the cache.
    #[clap(group = "cache", short = 't', long, display_order = 1)]
    pub(crate) invalid: bool,

    /// Display packages that don't have a tarball in the cache.
    #[clap(group = "cache", long, short, display_order = 1)]
    pub(crate) missing: bool,

    /// Packages to downgrade.
    pub(crate) packages: Vec<String>,
}

/// Open various webpages related to Aura.
#[derive(Parser, Debug)]
pub(crate) struct Open {
    /// Open the Aura Guide Book.
    #[clap(group = "open", long, short, display_order = 1)]
    pub(crate) docs: bool,

    /// Open Aura's Github repository.
    #[clap(group = "open", long, short, display_order = 1)]
    pub(crate) repo: bool,

    /// File a bug report for Aura.
    #[clap(group = "open", long, short, display_order = 1)]
    pub(crate) bug: bool,

    /// Open Aura's AUR page.
    #[clap(group = "open", long, short, display_order = 1)]
    pub(crate) aur: bool,
}

/// Output a dependency graph in DOT format.
#[derive(Parser, Debug)]
pub(crate) struct Deps {
    /// Display packages that depend on the given args.
    #[clap(long, display_order = 1)]
    pub(crate) reverse: bool,

    /// Include optional dependencies.
    #[clap(long, short, display_order = 1)]
    pub(crate) optional: bool,

    /// The number of layers up or down to allow.
    #[clap(long, display_order = 1)]
    pub(crate) limit: Option<u8>,

    /// Packages to focus on.
    pub(crate) packages: Vec<String>,
}

/// Validate your system.
#[derive(Parser, Debug)]
pub(crate) struct Check {}
