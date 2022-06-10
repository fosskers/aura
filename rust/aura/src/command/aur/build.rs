use crate::utils::ResultVoid;
use crate::{aura, proceed, red};
use colored::Colorize;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use log::debug;
use nonempty::NonEmpty;
use srcinfo::Srcinfo;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::process::Command;
use validated::Validated;

#[derive(FromVariants)]
pub enum Error {
    Srcinfo(srcinfo::Error),
    Io(std::io::Error),
    #[from_variants(skip)]
    Copies(NonEmpty<std::io::Error>),
    Utf8(std::str::Utf8Error),
    #[from_variants(skip)]
    FilenameExtraction(PathBuf),
    #[from_variants(skip)]
    TarballMove(PathBuf),
    Cancelled,
    #[from_variants(skip)]
    EditFail(PathBuf),
    Makepkg,
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
            Error::EditFail(pb) => write!(f, "Failed to edit {}", pb.display()),
        }
    }
}

// TODO Thu Jan 20 16:13:54 2022
//
// Consider parallel builds, but make it opt-in.
/// Build the given packages and yield paths to their built tarballs.
pub(crate) fn build<I, T>(
    fll: &FluentLanguageLoader,
    cache_d: &Path,
    build_d: &Path,
    hotedit: bool,
    editor: &str,
    pkg_clones: I,
) -> Result<Vec<PathBuf>, Error>
where
    I: Iterator<Item = T>,
    T: AsRef<Path>,
{
    aura!(fll, "A-build-prep");

    let to_install = pkg_clones
        .map(|path| build_one(fll, cache_d, path.as_ref(), hotedit, editor, build_d))
        .map(|r| build_check(fll, r))
        .collect::<Result<Vec<Vec<PathBuf>>, Error>>()?
        .into_iter()
        .flatten()
        .filter(|path| path.extension().map(|ex| ex != ".sig").unwrap_or(false))
        .collect();

    Ok(to_install)
}

fn build_one(
    fll: &FluentLanguageLoader,
    cache: &Path,
    clone: &Path,
    hotedit: bool,
    editor: &str,
    build_root: &Path,
) -> Result<Vec<PathBuf>, Error> {
    // --- Parse the .SRCINFO for metadata --- //
    let path = [clone, Path::new(".SRCINFO")].iter().collect::<PathBuf>();
    let info = Srcinfo::parse_file(path)?;
    let base = info.base.pkgbase;

    aura!(fll, "A-build-pkg", pkg = base.cyan().bold().to_string());

    // --- Prepare the Build Directory --- //
    let build = build_root.join(&base);
    std::fs::create_dir_all(&build)?;

    // --- Copy non-downloadable `source` files and PKGBUILD --- //
    let to_copy = info
        .base
        .source
        .iter()
        .flat_map(|av| av.vec.iter())
        .filter(|file| (file.contains("https://") || file.contains("http://")).not())
        .map(|s| s.as_str());

    let install_file: Option<PathBuf> = {
        let install = Path::new(&base).with_extension("install");
        clone.join(&install).is_file().then(|| install)
    };

    std::iter::once("PKGBUILD")
        .chain(install_file.iter().filter_map(|pb| pb.to_str()))
        .chain(to_copy)
        .map(|file| {
            debug!("Copying {}", file);
            let path = Path::new(&file);
            let source = clone.join(path);
            let target = build.join(path);
            std::fs::copy(source, target).void()
        })
        .collect::<Validated<(), std::io::Error>>()
        .ok()
        .map_err(Error::Copies)?;

    if hotedit {
        overwrite_build_files(fll, editor, &build)?;
    }

    let tarballs = makepkg(&build)?;
    for tb in tarballs.iter() {
        debug!("Built: {}", tb.display());
    }

    copy_to_cache(cache, &tarballs)
}

fn overwrite_build_files(
    fll: &FluentLanguageLoader,
    editor: &str,
    build_d: &Path,
) -> Result<(), Error> {
    if proceed!(fll, "A-build-hotedit-pkgbuild").is_some() {
        let pkgbuild = build_d.join("PKGBUILD");
        edit(editor, pkgbuild)?;
    }

    Ok(())
}

fn edit(editor: &str, file: PathBuf) -> Result<(), Error> {
    Command::new(editor)
        .arg(&file)
        .status()?
        .success()
        .then(|| ())
        .ok_or_else(|| Error::EditFail(file))?;

    Ok(())
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

    // NOTE Outputs absolute paths.
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
                    move_tarball(tarball, &target).map(|_| target)
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
            match proceed!(fll, "continue") {
                Some(_) => Ok(vec![]),
                None => Err(Error::Cancelled),
            }
        }
    }
}
