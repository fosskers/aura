use alpm::Db;
use alpm::{Alpm, SigLevel};
use clap::{crate_authors, crate_version, App, AppSettings, Arg};
use curl::easy::Easy;
use indicatif::ProgressBar;
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
            let packages: Vec<_> = matches
                .values_of("package")
                .map(|v| v.collect())
                .unwrap_or_else(|| Vec::new());

            if matches.is_present("info") {
                let conn: PooledConnection<AlpmManager> = pool.get().unwrap();
                let db = conn.localdb();
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
                packages
                    .into_par_iter()
                    .map_with(pool, |pul, pkg| {
                        let conn: PooledConnection<AlpmManager> = pul.get().unwrap();
                        let dbs = conn.syncdbs();
                        dbs.iter()
                            .filter_map(|db| package_url(&db, pkg).ok())
                            .next()
                    })
                    .filter_map(|url| url)
                    .for_each(|url| {
                        println!("Fetching {}", url);
                        let mut handle = Easy::new();
                        // handle.progress(true).map_err(Error::CURL)?;
                        // handle.url(&url).map_err(Error::CURL)?;
                        handle.progress(true).unwrap();
                        handle.url(&url).unwrap();
                        handle.perform().unwrap();
                        println!("Done {}", url);
                    });

                // // Set up the tarball download.
                // let total = pkg.download_size() as u64;
                // let pb = ProgressBar::new(total);
                // let mut handle = Easy::new();
                // handle.progress(true).map_err(Error::CURL)?;
                // handle.url(&url).map_err(Error::CURL)?;
                // handle
                //     .progress_function(move |_, dld, _, _| {
                //         let du = dld as u64;
                //         pb.set_position(du);
                //         if du == total {
                //             pb.finish_with_message("");
                //         }
                //         true
                //     })
                //     .map_err(Error::CURL)?;

                // // Download the package tarball.
                // handle.perform().map_err(Error::CURL)?;
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}

/// Given the name of some package, attempt to form the download URL of its tarball.
fn package_url(db: &Db, pkg: &str) -> Result<String, Error> {
    // Look up the package.
    let p = db.pkg(pkg).map_err(Error::ALPM)?;

    // Form the download URL.
    let db_str = p
        .db()
        .map(|db| db.name())
        .ok_or(Error::Other("Unknown DB!"))?;
    let arch = p.arch().ok_or(Error::Other("Unknown architecture!"))?;
    let url = format!(
        "https://mirror.f4st.host/archlinux/{}/os/{}/{}-{}-{}.pkg.tar.zst",
        db_str,
        arch,
        p.name(),
        p.version(),
        arch,
    );

    Ok(url)
}
