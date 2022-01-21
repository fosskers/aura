use crate::aura;
use colored::Colorize;
use i18n_embed::fluent::FluentLanguageLoader;
use srcinfo::Srcinfo;
use std::path::{Path, PathBuf};

pub enum Error {
    Srcinfo(srcinfo::Error),
}

impl From<srcinfo::Error> for Error {
    fn from(v: srcinfo::Error) -> Self {
        Self::Srcinfo(v)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Srcinfo(e) => write!(f, "{}", e),
        }
    }
}

// TODO Thu Jan 20 16:13:54 2022
//
// Consider parallel builds, but make it opt-in.
pub(crate) fn build<I, T>(
    fll: &FluentLanguageLoader,
    build_dir: &Path,
    pkg_clones: I,
) -> Result<(), Error>
where
    I: Iterator<Item = T>,
    T: AsRef<Path>,
{
    aura!(fll, "A-build-prep");

    for path in pkg_clones {
        build_one(path.as_ref())?;
    }

    Ok(())
}

fn build_one(clone: &Path) -> Result<(), Error> {
    let path = [clone, Path::new(".SRCINFO")].iter().collect::<PathBuf>();
    let info = Srcinfo::parse_file(path)?;
    let base = info.base.pkgbase;

    for s in info.base.source.into_iter().flat_map(|av| av.vec) {
        println!("Source ({}): {}", base, s);
    }

    Ok(())
}
