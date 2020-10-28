use std::sync::Arc;
use std::thread;

use alpm::{Alpm, Package, SigLevel};
use clap::{crate_authors, crate_version, App, AppSettings, Arg};
use curl::easy::Easy;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use r2d2::{ManageConnection, Pool, PooledConnection};
use rayon::prelude::*;

#[derive(Debug)]
enum Error {
    ALPM(alpm::Error),
    CURL(curl::Error),
    R2D2(r2d2::Error),
    Other(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::ALPM(e) => e.fmt(f),
            Error::CURL(e) => e.fmt(f),
            Error::R2D2(e) => e.fmt(f),
            Error::Other(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ALPM(e) => Some(e),
            Error::CURL(e) => Some(e),
            Error::R2D2(e) => Some(e),
            Error::Other(_) => None,
        }
    }
}

// TODO Consider string slices later.
struct AlpmManager {
    root: String,
    db_path: String,
    sync_dbs: Vec<String>,
}

impl Default for AlpmManager {
    fn default() -> Self {
        AlpmManager {
            root: "/".to_string(),
            db_path: "/var/lib/pacman/".to_string(),
            sync_dbs: vec![
                "core".to_string(),
                "extra".to_string(),
                "community".to_string(),
                "multilib".to_string(),
            ],
        }
    }
}

impl ManageConnection for AlpmManager {
    type Connection = alpm::Alpm;
    type Error = alpm::Error;

    fn connect(&self) -> Result<Self::Connection, Self::Error> {
        // From `libalpm.h`:
        //
        // > Initializes the library.
        // > Creates handle, connects to database and creates lockfile.
        // > This must be called before any other functions are called.
        let alpm = Alpm::new(self.root.clone(), self.db_path.clone())?;
        self.sync_dbs
            .iter()
            .map(|s| alpm.register_syncdb(s, SigLevel::USE_DEFAULT))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(alpm)
    }

    // Are we still connected to the database? Docs say that it's not
    // unreasonable to actually try a query to see what happens.
    fn is_valid(&self, _: &mut Self::Connection) -> Result<(), Self::Error> {
        Ok(())
    }

    // TODO This is supposed to be a fast check. What does libalpm provide?
    fn has_broken(&self, _: &mut Self::Connection) -> bool {
        false
    }
}

fn main() -> Result<(), Error> {
    let manager = AlpmManager::default();
    let pool = Pool::builder()
        .max_size(4)
        .build(manager)
        .map_err(Error::R2D2)?;
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
            let pkg_names: Vec<_> = matches
                .values_of("package")
                .map(|v| v.collect())
                .unwrap_or_else(|| Vec::new());

            if matches.is_present("info") {
                let conn: PooledConnection<AlpmManager> = pool.get().unwrap();
                let db = conn.localdb();
                match pkg_names.as_slice() {
                    // Fetch all packages.
                    [] => {
                        for p in db.pkgs() {
                            println!("{} - {}", p.name(), p.version());
                        }
                    }
                    // Do lookups for each package.
                    _ => {
                        for p in pkg_names {
                            let pkg = db.pkg(p).map_err(Error::ALPM)?;
                            println!("{} - {}", pkg.name(), pkg.version());
                        }
                    }
                }
            } else {
                // Initialize the download progress display.
                let marc = Arc::new(MultiProgress::new());
                let marc1 = marc.clone();
                thread::spawn(move || marc1.join());

                let packages: Vec<(String, u64)> = pkg_names
                    .into_par_iter()
                    .map_with(pool, |pul, pkg| {
                        let conn: PooledConnection<AlpmManager> = pul.get().unwrap();
                        let dbs = conn.syncdbs();
                        dbs.iter()
                            .filter_map(|db| db.pkg(pkg).ok())
                            .filter_map(|p| {
                                let total = p.download_size() as u64;
                                package_url(&p).ok().map(|u| (u, total))
                            })
                            .next()
                    })
                    .filter_map(|o| o)
                    .collect();

                // Set up download progress display.
                let m = MultiProgress::new();
                let spinners: Vec<ProgressBar> = packages
                    .iter()
                    .map(|(_, total)| {
                        let pb = ProgressBar::new(*total);
                        let style = ProgressStyle::default_bar()
                            .template("{spinner:.green} [{wide_bar:.cyan/blue}] {bytes}/{total_bytes} ({eta})")
                            .progress_chars("#>-");
                        pb.set_style(style);
                        m.add(pb)
                    })
                    .collect();
                thread::spawn(move || m.join());

                packages
                    .into_par_iter()
                    .zip(spinners)
                    .for_each(|((url, _), pb)| {
                        // TODO Could cache the `Easy` sessions and use
                        // `Easy::reset` between each use!
                        let mut handle = Easy::new();
                        // handle.progress(true).map_err(Error::CURL)?;
                        // handle.url(&url).map_err(Error::CURL)?;
                        handle.progress(true).unwrap();
                        handle.url(&url).unwrap();
                        handle
                            .progress_function(move |_, dld, _, _| {
                                let du = dld as u64;
                                pb.set_position(du);
                                true
                            })
                            .unwrap();

                        handle.perform().unwrap();
                    });
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

/// Given the name of some package, attempt to form the download URL of its tarball.
fn package_url(pkg: &Package) -> Result<String, Error> {
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

    Ok(url)
}
