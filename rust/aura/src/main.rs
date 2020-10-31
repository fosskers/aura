use aura::flags::{SubCmd, ToArgs};
use clap::Clap;
use std::process::Command;

#[derive(Debug)]
enum Error {
    IO(std::io::Error),
}

fn main() -> Result<(), Error> {
    let args = aura::flags::Args::parse();

    match args.subcmd {
        SubCmd::Sync(s) => {
            let back = s.to_args();
            Command::new("pacman")
                .args(back)
                .status()
                .map_err(Error::IO)?;
        }
        _ => {}
    }

    Ok(())
    // println!("{:#?}", args);
}
