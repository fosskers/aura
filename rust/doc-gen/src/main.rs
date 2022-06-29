use aura::flags::Args;
use clap::CommandFactory;
use clap_mangen::Man;
use std::fs::File;

fn main() -> std::io::Result<()> {
    let man = Man::new(Args::command());
    let mut file = File::create("aura.8")?;

    man.render(&mut file)?;
    Ok(())
}
