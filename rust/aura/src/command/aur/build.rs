use crate::utils::ResultVoid;
use crate::{a, aura, red};
use colored::Colorize;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::debug;
use nonempty::NonEmpty;
use srcinfo::Srcinfo;
use std::{
    ops::Not,
    path::{Path, PathBuf},
    process::Command,
};
use validated::Validated;

pub enum Error {
    Srcinfo(srcinfo::Error),
    Io(std::io::Error),
    Copies(NonEmpty<std::io::Error>),
    Utf8(std::str::Utf8Error),
    FilenameExtraction(PathBuf),
    TarballMove(PathBuf),
    Cancelled,
    Makepkg,
}

impl From<std::str::Utf8Error> for Error {
    fn from(v: std::str::Utf8Error) -> Self {
        Self::Utf8(v)
    }
}

impl From<std::io::Error> for Error {
    fn from(v: std::io::Error) -> Self {
        Self::Io(v)
    }
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
            Error::Io(e) => write!(f, "{}", e),
            Error::Copies(es) => {
                for e in es {
                    writeln!(f, "{}", e)?;
                }
                Ok(())
            }
            Error::Makepkg => write!(f, "makepkg failed"),
            Error::Utf8(e) => write!(f, "{}", e),
            Error::FilenameExtraction(pb) => {
                write!(
                    f,
                    "ANOMALY: Failed to extract filename from {}",
                    pb.display()
                )
            }
            Error::TarballMove(pb) => write!(f, "Failed to move {}", pb.display()),
            Error::Cancelled => write!(f, "Not proceeding with build."),
        }
    }
}

// TODO Thu Jan 20 16:13:54 2022
//
// Consider parallel builds, but make it opt-in.
/// Build the given packages and yield paths to their built tarballs.
pub(crate) fn build<I, T>(
    fll: &FluentLanguageLoader,
    cache_dir: &Path,
    build_dir: &Path,
    pkg_clones: I,
) -> Result<Vec<PathBuf>, Error>
where
    I: Iterator<Item = T>,
    T: AsRef<Path>,
{
    aura!(fll, "A-build-prep");

    let to_install = pkg_clones
        .map(|path| build_one(cache_dir, path.as_ref(), build_dir))
        .map(|r| build_check(fll, r))
        .collect::<Result<Vec<Vec<PathBuf>>, Error>>()?
        .into_iter()
        .flatten()
        .filter(|path| path.extension().map(|ex| ex != ".sig").unwrap_or(false))
        .collect();

    Ok(to_install)
}

fn build_one(cache: &Path, clone: &Path, build_root: &Path) -> Result<Vec<PathBuf>, Error> {
    // --- Parse the .SRCINFO for metadata --- //
    let path = [clone, Path::new(".SRCINFO")].iter().collect::<PathBuf>();
    let info = Srcinfo::parse_file(path)?;
    let base = info.base.pkgbase;

    debug!("Building {}", base);

    // --- Prepare the Build Directory --- //
    let build = [build_root, Path::new(&base)].iter().collect::<PathBuf>();
    std::fs::create_dir_all(&build)?;

    // --- Copy non-downloadable `source` files and PKGBUILD --- //
    let to_copy = info
        .base
        .source
        .iter()
        .flat_map(|av| av.vec.iter())
        .filter(|file| (file.contains("https://") || file.contains("http://")).not())
        .map(|s| s.as_str());

    std::iter::once("PKGBUILD")
        .chain(to_copy)
        .map(|file| {
            debug!("Copying {}", file);
            let path = Path::new(&file);
            let source = [clone, path].iter().collect::<PathBuf>();
            let target = [&build, path].iter().collect::<PathBuf>();
            std::fs::copy(source, target).void()
        })
        .collect::<Validated<(), std::io::Error>>()
        .ok()
        .map_err(Error::Copies)?;

    let tarballs = makepkg(&build)?;
    for tb in tarballs.iter() {
        debug!("Built: {}", tb.display());
    }

    copy_to_cache(cache, &tarballs)
}

/// Build each package specified by the `PKGBUILD` and yield a list of the built
/// tarballs.
fn makepkg(within: &Path) -> Result<Vec<PathBuf>, Error> {
    Command::new("makepkg")
        .arg("-f") // TODO Remove or rethink
        .current_dir(within)
        .status()?
        .success()
        .then(|| ())
        .ok_or(Error::Makepkg)?;

    let bytes = Command::new("makepkg")
        .arg("--packagelist")
        .current_dir(within)
        .output()?
        .stdout;

    let tarballs = std::str::from_utf8(&bytes)?
        .lines()
        .map(|line| [line].iter().collect())
        .collect();

    Ok(tarballs)
}

/// Copy each built package tarball (and any signature files) to the package
/// cache, and return a list of the moved files.
fn copy_to_cache(cache: &Path, tarballs: &[PathBuf]) -> Result<Vec<PathBuf>, Error> {
    tarballs
        .iter()
        .map(|tarball| {
            tarball
                .file_name()
                .ok_or_else(|| Error::FilenameExtraction(tarball.clone()))
                .and_then(|file| {
                    let target = [cache, file.as_ref()].iter().collect::<PathBuf>();
                    move_tarball(&tarball, &target).map(|_| target)
                })
        })
        .collect::<Result<Vec<PathBuf>, Error>>()
}

/// Move a given [`Path`] to some other location. We do this because renames
/// fail if the target location is on a different mount point, while moves do
/// not.
fn move_tarball(source: &Path, target: &Path) -> Result<(), Error> {
    Command::new("mv")
        .arg(source)
        .arg(target)
        .status()?
        .success()
        .then(|| ())
        .ok_or_else(|| Error::TarballMove(source.to_path_buf()))
}

fn build_check(
    fll: &FluentLanguageLoader,
    r: Result<Vec<PathBuf>, Error>,
) -> Result<Vec<PathBuf>, Error> {
    match r {
        Ok(tbs) => Ok(tbs),
        Err(e) => {
            red!(fll, "A-build-fail");
            eprintln!("\n  {}\n", e);
            let msg = format!("{} {} ", fl!(fll, "continue"), fl!(fll, "proceed-yes"));
            match crate::utils::prompt(&a!(msg)) {
                Some(_) => Ok(vec![]),
                None => Err(Error::Cancelled),
            }
        }
    }
}
