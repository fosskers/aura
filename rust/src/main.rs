use alpm::{Alpm, Package, SigLevel};
use clap::{crate_version, AppSettings, Clap};
use curl::easy::Easy;
use fluent::{FluentBundle, FluentResource};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use r2d2::{ManageConnection, Pool, PooledConnection};
use rayon::prelude::*;
use std::thread;
use unic_langid::langid;

#[derive(Debug)]
enum Error {
    // ALPM(alpm::Error),
    // CURL(curl::Error),
    R2D2(r2d2::Error),
    Other(&'static str),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // Error::ALPM(e) => e.fmt(f),
            // Error::CURL(e) => e.fmt(f),
            Error::R2D2(e) => e.fmt(f),
            Error::Other(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            // Error::ALPM(e) => Some(e),
            // Error::CURL(e) => Some(e),
            Error::R2D2(e) => Some(e),
            Error::Other(_) => None,
        }
    }
}

#[derive(Clap, Debug)]
#[clap(version = crate_version!(),
       author = "Colin Woodbury",
       about = "Install and manage Arch Linux and AUR packages",
       setting = AppSettings::VersionlessSubcommands)]
struct Args {
    /// Output in English.
    #[clap(group = "lang", long, global = true)]
    english: bool,
    /// Output in Japanese.
    #[clap(group = "lang", long, global = true)]
    japanese: bool,
    /// Less verbose output.
    #[clap(long, short, global = true)]
    quiet: bool,
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap, Debug)]
enum SubCommand {
    Sync(Sync),
}

/// Synchronize packages.
#[derive(Clap, Debug)]
#[clap(short_flag = 'S', long_flag = "sync")]
struct Sync {
    #[clap(long, short, about = "Search for packages via REGEX.")]
    search: bool,
    #[clap(long, short, about = "Look up a specific package.")]
    info: bool,
    #[clap(about = "Packages to search/install.")]
    packages: Vec<String>,
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
    // Parse commandline args before anything else.
    let args = Args::parse();

    // Pooled connections to ALPM.
    let manager = AlpmManager::default();
    let pool = Pool::builder()
        .max_size(4) // TODO Should be the number of CPU cores available.
        .build(manager)
        .map_err(Error::R2D2)?;

    // Localization Settings
    let english = langid!("en-US");
    let japanese = langid!("ja-JP");
    let en_msg = "downloading = Downloading tarballs...";
    let ja_msg = "downloading = ターボールをダウンロード中…";
    let en_res = FluentResource::try_new(en_msg.to_owned())
        .map_err(|_| Error::Other("Couldn't parse English localisations."))?;
    let ja_res = FluentResource::try_new(ja_msg.to_owned())
        .map_err(|_| Error::Other("Couldn't parse Japanese localisations."))?;

    let bundle = if args.japanese {
        let mut bundle = FluentBundle::new(&[japanese]);
        bundle
            .add_resource(&ja_res)
            .map_err(|_| Error::Other("Failed to add Japanese to bundle."))?;
        bundle
    } else {
        let mut bundle = FluentBundle::new(&[english]);
        bundle
            .add_resource(&en_res)
            .map_err(|_| Error::Other("Failed to add English to bundle."))?;
        bundle
    };

    match args.subcmd {
        SubCommand::Sync(s) if s.info => println!("Info!"),
        SubCommand::Sync(s) if s.search => println!("Search!"),
        SubCommand::Sync(s) => {
            // Display localized message.
            let pat = bundle.get_message("downloading").unwrap().value.unwrap();
            let mut err = vec![];
            let msg = bundle.format_pattern(&pat, None, &mut err);
            println!("{}", msg);

            // Look up each package in the ALPM database.
            let packages: Vec<(String, String, u64)> = s
                .packages
                .par_iter()
                .map_with(pool, |pul, pkg| {
                    let conn: PooledConnection<AlpmManager> = pul.get().unwrap();
                    let dbs = conn.syncdbs();
                    dbs.iter()
                        .filter_map(|db| db.pkg(pkg).ok())
                        .filter_map(|p| {
                            let total = p.download_size() as u64;
                            package_url(&p)
                                .ok()
                                .map(|u| (p.name().to_string(), u, total))
                        })
                        .next()
                })
                .filter_map(|o| o)
                .collect();

            // Set up download progress display.
            let m = MultiProgress::new();
            let spinners: Vec<ProgressBar> = packages
                .iter()
                .map(|(p_name, _, total)| {
                    let pb = ProgressBar::new(*total);
                    // TODO Make the spacing dynamic based on the package names.
                    let template = format!(
                        "{:<8} [{{wide_bar:.cyan/blue}}] {{bytes}}/{{total_bytes}} ({{eta}})",
                        p_name
                    );
                    let style = ProgressStyle::default_bar()
                        .template(&template)
                        .progress_chars("#>-");
                    pb.set_style(style);
                    m.add(pb)
                })
                .collect();
            thread::spawn(move || m.join());

            // Download the packages concurrently while displaying progress.
            packages
                .into_par_iter()
                .zip(spinners)
                .for_each(|((_, url, total), pb)| {
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
                            if du == total {
                                pb.finish();
                            }
                            true
                        })
                        .unwrap();

                    handle.perform().unwrap();
                });
        }
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
