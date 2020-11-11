use clap::{crate_version, AppSettings, Clap};
use simplelog::LevelFilter;

/// Subcommands which can be reduced back into their raw argument form for
/// passing to Pacman.
pub trait ToArgs {
    fn to_args(&self) -> Vec<&str>;
}

/// Commandline arguments to the Aura executable.
#[derive(Clap, Debug)]
#[clap(version = crate_version!(),
       author = "Colin Woodbury",
       about = "Install and manage Arch Linux and AUR packages",
       setting = AppSettings::VersionlessSubcommands,
       // setting = AppSettings::UnifiedHelpMessage, // TODO Is this broken?
       setting = AppSettings::DisableHelpSubcommand)]
pub struct Args {
    /// Output in English.
    #[clap(group = "lang", long, global = true)]
    pub english: bool,
    /// Output in Japanese.
    #[clap(group = "lang", long, global = true, alias = "日本語")]
    pub japanese: bool,
    /// Minimum level of Aura log messages to display.
    #[clap(long, value_name = "level", possible_values = &["debug", "info", "warn", "error"], global = true)]
    pub log_level: Option<LevelFilter>,
    #[clap(subcommand)]
    pub subcmd: SubCmd,
}

#[derive(Clap, Debug)]
pub enum SubCmd {
    // --- Pacman Commands --- //
    Database(Database),
    Files(Files),
    Query,
    Remove(Remove),
    Sync(Sync),
    DepTest(DepTest),
    Upgrade(Upgrade),
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
    #[clap(long, short = 'b', value_name = "path", display_order = 1)]
    dbpath: Option<String>,
    /// Remove old packages from cache directory (-cc for all).
    #[clap(long, short, multiple_occurrences = true, display_order = 1)]
    clean: bool,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', display_order = 1)]
    nodeps: bool,
    /// View all members of a package group (-gg to view all groups and members).
    #[clap(long, short, display_order = 1)]
    groups: bool,
    /// View package information (-ii for extended information).
    #[clap(long, short, display_order = 1)]
    info: bool,
    /// View a list of packages in a repo.
    #[clap(long, short, display_order = 1)]
    list: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 1)]
    print: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", display_order = 1)]
    root: Option<String>,
    /// Search remote repositories for matchings strings.
    #[clap(long, short, display_order = 1)]
    search: bool,
    /// Upgrade installed packages (-uu enables downgrades).
    #[clap(long, short = 'u', display_order = 1)]
    sysupgrade: bool,
    /// Be verbose.
    #[clap(long, short, display_order = 1)]
    verbose: bool,
    /// Download packages but do not install/upgrade anything.
    #[clap(long, short = 'w', display_order = 1)]
    downloadonly: bool,
    /// Download fresh package databases from the server (-yy to force a refresh even if up to date).
    #[clap(long, short = 'y', display_order = 1)]
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
    /// Operate on a mounted guest system (root-only).
    #[clap(long)]
    sysroot: bool,
    /// Packages to search/install.
    packages: Vec<String>,
}

impl ToArgs for Sync {
    fn to_args(&self) -> Vec<&str> {
        let mut args = vec!["-S"];

        if self.clean {
            args.push("-c");
        }

        if self.downloadonly {
            args.push("-w");
        }

        if self.groups {
            args.push("-g");
        }

        if self.info {
            args.push("-i");
        }

        if self.list {
            args.push("-l");
        }

        if self.nodeps {
            args.push("-d");
        }

        if self.print {
            args.push("-p");
        }

        if self.quiet {
            args.push("-q");
        }

        if self.refresh {
            args.push("-y");
        }

        if self.search {
            args.push("-s");
        }

        if self.sysupgrade {
            args.push("-u");
        }

        if self.verbose {
            args.push("-b");
        }

        if self.asdeps {
            args.push("--asdeps");
        }

        for p in &self.packages {
            args.push(p);
        }

        args
    }
}

// TODO Reconcile `pacman -Th` and the manpage entry for -T.
// TODO Is it possible to disable subcommand "plan names"? i.e. to have only
// their long/short variants remain valid (or at least shown in `-h`).
/// Check if given dependencies are satisfied.
#[derive(Clap, Debug)]
#[clap(short_flag = 'T', long_flag = "deptest")]
pub struct DepTest {
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path", display_order = 1)]
    dbpath: Option<String>,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", display_order = 1)]
    root: Option<String>,
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path", display_order = 1)]
    dbpath: Option<String>,
    /// Skip dependency version checks (-dd to skip all checks).
    #[clap(long, short = 'd', display_order = 1)]
    nodeps: bool,
    /// Print the targets instead of performing the operation.
    #[clap(long, short, display_order = 1)]
    print: bool,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", display_order = 1)]
    root: Option<String>,
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path", display_order = 1)]
    dbpath: Option<String>,
    /// View a list of packages in a repo.
    #[clap(long, short, display_order = 1)]
    list: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", display_order = 1)]
    root: Option<String>,
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path", display_order = 1)]
    dbpath: Option<String>,
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
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", display_order = 1)]
    root: Option<String>,
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
    /// Set an alternate database location.
    #[clap(long, short = 'b', value_name = "path", display_order = 1)]
    dbpath: Option<String>,
    /// Test local database for validity (-kk for sync databases).
    #[clap(long, short = 'k', multiple_occurrences = true, display_order = 1)]
    check: bool,
    /// Show less information for query and search.
    #[clap(long, short, display_order = 1)]
    quiet: bool,
    /// Set an alternate installation root.
    #[clap(long, short, value_name = "path", display_order = 1)]
    root: Option<String>,
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
