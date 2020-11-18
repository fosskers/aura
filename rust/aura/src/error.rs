/// Error type for all issues that can occur in the Aura library or executable.
#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Arch(aura_arch::Error),
    Alpm(alpm::Error),
    RustyLine(rustyline::error::ReadlineError),
}
