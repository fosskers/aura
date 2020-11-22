//! Types and utilities for parsing flags from the command line.

use clap::{crate_version, AppSettings, Clap};
use simplelog::LevelFilter;
use unic_langid::{langid, LanguageIdentifier};

/// Commandline arguments to the Aura executable.
#[derive(Clap, Debug)]
#[clap(version = crate_version!(),
       author = "Colin Woodbury",
       about = "Install and manage Arch Linux and AUR packages",
       setting = AppSettings::VersionlessSubcommands,
       // setting = AppSettings::UnifiedHelpMessage, // TODO Is this broken?
       setting = AppSettings::DisableHelpSubcommand)]
pub struct Args {
    // --- Global Pacman Options --- //
    /// Set an alternate database location.
    #[clap(
        long,
        short = 'b',
        value_name = "path",
        global = true,
        display_order = 1
    )]
    pub dbpath: Option<String>,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", global = true, display_order = 1)]
    pub root: Option<String>,

    // --- Aura Language Options --- //
    /// Output in English.
    #[clap(group = "lang", long, global = true)]
    pub english: bool,
    /// Output in Japanese.
    #[clap(group = "lang", long, global = true, alias = "日本語")]
    pub japanese: bool,

    // --- Other Aura Options --- //
    /// Minimum level of Aura log messages to display.
    #[clap(long, value_name = "level", possible_values = &["debug", "info", "warn", "error"], global = true)]
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
            _ if self.japanese => Some(langid!("ja-JP")),
            _ => None,
        }
    }
}

/// The Aura Package Manager.
#[derive(Clap, Debug)]
pub enum SubCmd {
    // --- Pacman Commands --- //
    /// Operate on the package database.
    Database(Database),
    /// Query the files database.
    Files(Files),
    /// Query the package database.
    Query(Query),
    /// Remove packages from the system.
    Remove(Remove),
    /// Synchronize packages.
    Sync(Sync),
    /// Check if given dependencies are satisfied.
    DepTest(DepTest),
    /// Upgrade or add packages to the system.
    Upgrade(Upgrade),
    // --- Aura Commands --- //
    /// Synchronize AUR packages.
    AurSync,
    /// Save and restore the global package state.
    Backup,
    /// Manage the package cache.
    Cache,
    /// View the Pacman/ALPM log.
    Log,
    /// Handle orphan packages.
    Orphans(Orphans),
    /// Perform security analysis of a PKGBUILD.
    Analysis(Analysis),
    /// Display Aura's available localizations
    Languages,
    /// View your pacman.conf.
    ViewConf,
    // Extra, // TODO completions, dot output, etc?
}

/// Synchronize packages.
#[derive(Clap, Debug)]
#[clap(short_flag = 'S', long_flag = "sync")]
pub struct Sync {
    /// Remove old packages from cache directory (-cc for all).
    #[clap(long, short, parse(from_occurrences), display_order = 1)]
    pub clean: u8,
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
#[derive(Clap, Debug)]
#[clap(short_flag = 'T', long_flag = "deptest")]
pub struct DepTest {
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
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
    logfile: Option<String>,
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
#[derive(Clap, Debug)]
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
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to install, either a tarball or a URL.
    packages: Vec<String>,
}

// TODO `pacman -Fh` does not include the top-level usage line!
/// Query the files database.
#[derive(Clap, Debug)]
#[clap(short_flag = 'F', long_flag = "files")]
pub struct Files {
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
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<String>,
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
#[derive(Clap, Debug)]
#[clap(short_flag = 'R', long_flag = "remove")]
pub struct Remove {
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
    /// Set an alternate log file.
    #[clap(long, value_name = "path")]
    logfile: Option<String>,
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
#[derive(Clap, Debug)]
#[clap(short_flag = 'D', long_flag = "database")]
pub struct Database {
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
    logfile: Option<String>,
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
#[derive(Clap, Debug)]
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
    /// List outdated pacakges [filter].
    #[clap(long, short, display_order = 1)]
    upgrades: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Set an alternate architecture.
    #[clap(long)]
    arch: Option<String>,
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
    logfile: Option<String>,
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
#[derive(Clap, Debug)]
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
#[derive(Clap, Debug)]
#[clap(short_flag = 'O', long_flag = "orphans")]
pub struct Orphans {
    /// Mark a package as being explicitly installed.
    #[clap(group = "orphans", long, short, value_name = "packages")]
    pub adopt: Vec<String>,
    /// Uninstall all orphan packages.
    #[clap(group = "orphans", long, short = 'j')]
    pub abandon: bool,
}
