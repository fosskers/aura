use clap::{crate_authors, crate_version, App, AppSettings, Arg};

fn main() {
    let v = format!("{} - libalpm {}", crate_version!(), alpm::version());

    let args = App::new("aura")
        .author(crate_authors!())
        .version(crate_version!())
        .long_version(&*v)
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
                        .multiple_values(true)
                        .value_name("PACKAGE")
                        .about("search remote repositories for matching strings"),
                )
                .arg(
                    Arg::new("info")
                        .short('i')
                        .long("info")
                        .conflicts_with("search")
                        .multiple_values(true)
                        .value_name("PACKAGE")
                        .about("view package information"),
                )
                .arg(
                    Arg::new("package")
                        .about("Packages to install.")
                        .multiple(true)
                        .required_unless_present("search")
                        .takes_value(true),
                ),
        )
        .get_matches();

    println!("{:?}", args);

    println!("{}", alpm::version());
}
