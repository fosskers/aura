/// Error type for all issues that can occur in the Aura library or executable.
#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Arch(aura_arch::Error),
    Alpm(alpm::Error),
    RustyLine(rustyline::error::ReadlineError),
    /// The said "no" at some prompt.
    Rejected,
    /// None of the packages specified by the user actually exist.
    NoneExist,
}
