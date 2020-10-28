use alpm::{Alpm, SigLevel};
use clap::{crate_authors, crate_version, App, AppSettings, Arg};
use curl::easy::Easy;
use indicatif::ProgressBar;

#[derive(Debug)]
enum Error {
    ALPM(alpm::Error),
    CURL(curl::Error),
    Other(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::ALPM(e) => e.fmt(f),
            Error::CURL(e) => e.fmt(f),
            Error::Other(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ALPM(e) => Some(e),
            Error::CURL(e) => Some(e),
            Error::Other(_) => None,
        }
    }
}

fn main() -> Result<(), Error> {
    let alpm = Alpm::new("/", "/var/lib/pacman/").map_err(Error::ALPM)?;
    let ver: &str = &format!("{} - libalpm {}", crate_version!(), alpm::version());

    let args = App::new("aura")
        .author(crate_authors!())
        .version(ver)
        .about("Install and manage Arch Linux and AUR packages.")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        // .subcommand(
        //     App::new("query")
        //         .short_flag('Q')
        //         .long_flag("query")
        //         .about("Query the package database.")
        //         .arg(
        //             Arg::new("search")
        //                 .short('s')
        //                 .long("search")
        //                 .about("Search locally installed packages.")
        //                 .multiple_values(true),
        //         )
        //         .arg(
        //             Arg::new("info")
        //                 .long("info")
        //                 .short('i')
        //                 .conflicts_with("search")
        //                 .about("view package information")
        //                 .multiple_values(true),
        //         ),
        // )
        .subcommand(
            App::new("sync")
                .short_flag('S')
                .long_flag("sync")
                .about("Synchronize packages.")
                .arg(Arg::new("quiet").long("quiet").short('q').about("Shhh"))
                .arg(
                    Arg::new("search")
                        .short('s')
                        .long("search")
                        .conflicts_with("info")
                        .about("Search repositories for a matching regex."),
                )
                .arg(
                    Arg::new("info")
                        .short('i')
                        .long("info")
                        .conflicts_with("search")
                        .about("View package information."),
                )
                .arg(
                    Arg::new("package")
                        .about("Packages to install.")
                        .multiple(true)
                        // .required_unless_present("search")
                        .takes_value(true),
                ),
        )
        .get_matches();

    match args.subcommand() {
        Some(("sync", matches)) => {
            // The actual package names.
            let packages: Vec<_> = matches
                .values_of("package")
                .map(|v| v.collect())
                .unwrap_or_else(|| Vec::new());

            if matches.is_present("info") {
                let db = alpm.localdb();
                match packages.as_slice() {
                    // Fetch all packages.
                    [] => {
                        for p in db.pkgs() {
                            println!("{} - {}", p.name(), p.version());
                        }
                    }
                    // Do lookups for each package.
                    _ => {
                        for p in packages {
                            let pkg = db.pkg(p).map_err(Error::ALPM)?;
                            println!("{} - {}", pkg.name(), pkg.version());
                        }
                    }
                }
            } else {
                // Look up the package.
                let sync_dbs = init_sync_dbs(&alpm).map_err(Error::ALPM)?;
                let p = packages
                    .first()
                    .ok_or_else(|| Error::Other("No packages!"))?;
                println!("Looking for {}...", p);
                println!("# of Sync DBs: {}", sync_dbs.len());
                let pkg = sync_dbs
                    .into_iter()
                    .filter_map(|db| db.pkg(*p).ok())
                    .next()
                    .ok_or_else(|| Error::Other("No db matches!"))?;

                // Form the download URL.
                let db_str = pkg
                    .db()
                    .map(|db| db.name())
                    .ok_or(Error::Other("Unknown DB!"))?;
                let arch = pkg.arch().ok_or(Error::Other("Unknown architecture!"))?;
                let url = format!(
                    "https://mirror.f4st.host/archlinux/{}/os/{}/{}-{}-{}.pkg.tar.zst",
                    db_str,
                    arch,
                    pkg.name(),
                    pkg.version(),
                    arch,
                );

                // Set up the tarball download.
                let total = pkg.download_size() as u64;
                let pb = ProgressBar::new(total);
                let mut handle = Easy::new();
                handle.progress(true).map_err(Error::CURL)?;
                handle.url(&url).map_err(Error::CURL)?;
                handle
                    .progress_function(move |_, dld, _, _| {
                        pb.set_position(dld as u64);
                        true
                    })
                    .map_err(Error::CURL)?;

                // Download the package tarball.
                handle.perform().map_err(Error::CURL)?;
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

/// Handles to the official repos must be initialized before they can be
/// queried.
fn init_sync_dbs(alpm: &Alpm) -> alpm::Result<Vec<alpm::Db>> {
    let names = vec!["core", "extra", "community"];

    names
        .into_iter()
        .map(|s| alpm.register_syncdb(s, SigLevel::USE_DEFAULT))
        .collect()
}
