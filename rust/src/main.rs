use alpm::Alpm;
use clap::{crate_authors, crate_version, App, AppSettings, Arg};

fn main() -> alpm::Result<()> {
    let alpm = Alpm::new("/", "/var/lib/pacman/")?;
    let db = alpm.localdb();
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
            if matches.is_present("info") {
                let packages: Vec<_> = matches
                    .values_of("package")
                    .map(|v| v.collect())
                    .unwrap_or_else(|| Vec::new());

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
                            let pkg = db.pkg(p)?;
                            println!("{} - {}", pkg.name(), pkg.version());
                        }
                    }
                }
            } else {
                println!("Installing...");
            }
        }
        _ => unreachable!(),
    }

    Ok(())
}
