use crate::utils::ResultVoid;
use crate::{aura, proceed, red};
use colored::Colorize;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use log::{debug, warn};
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
    Git(aura_core::git::Error),
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
            Error::Git(e) => write!(f, "{e}"),
        }
    }
}

/// The results of a successful build.
pub(crate) struct Built {
    pub(crate) clone: PathBuf,
    pub(crate) tarballs: Vec<PathBuf>,
}

// TODO Thu Jan 20 16:13:54 2022
//
// Consider parallel builds, but make it opt-in.
/// Build the given packages and yield paths to their built tarballs.
pub(crate) fn build<I>(
    fll: &FluentLanguageLoader,
    cache_d: &Path,
    build_d: &Path,
    hashes_d: &Path,
    diff: bool,
    hotedit: bool,
    editor: &str,
    pkg_clones: I,
) -> Result<Vec<Built>, Error>
where
    I: Iterator<Item = PathBuf>,
{
    aura!(fll, "A-build-prep");

    let to_install = pkg_clones
        .map(|path| build_one(fll, cache_d, hashes_d, path, diff, hotedit, editor, build_d))
        .map(|r| build_check(fll, r))
        .collect::<Result<Vec<Option<Built>>, Error>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok(to_install)
}

fn build_one(
    fll: &FluentLanguageLoader,
    cache: &Path,
    hashes: &Path,
    clone: PathBuf,
    diff: bool,
    hotedit: bool,
    editor: &str,
    build_root: &Path,
) -> Result<Built, Error> {
    // --- Parse the .SRCINFO for metadata --- //
    let path = clone.join(".SRCINFO");
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

    if diff {
        show_diffs(fll, hashes, &clone, &base)?;
    }

    if hotedit {
        overwrite_build_files(fll, editor, &build, &base)?;
    }

    let tarballs = {
        let tarballs = makepkg(&build)?;

        for tb in tarballs.iter() {
            debug!("Built: {}", tb.display());
        }

        // --- Copy all build artifacts to the cache, and then ignore any sig files --- //
        copy_to_cache(cache, &tarballs)?
            .into_iter()
            .filter(|path| path.extension().map(|ex| ex != "sig").unwrap_or(false))
            .collect::<Vec<_>>()
    };

    Ok(Built { clone, tarballs })
}

fn show_diffs(
    fll: &FluentLanguageLoader,
    hashes: &Path,
    clone: &Path,
    pkgbase: &str,
) -> Result<(), Error> {
    // Silently skip over any hashes that couldn't be read. This is mostly
    // likely due to the package being installed for the first time, thus having
    // no history to compare to.
    match hash_of_last_install(hashes, pkgbase) {
        Err(e) => warn!("Couldn't read latest hash of {}: {}", pkgbase, e),
        Ok(hash) => {
            if proceed!(fll, "A-build-diff").is_some() {
                aura_core::git::diff(clone, &hash)?;
            }
        }
    }

    Ok(())
}

/// What AUR repo git hash is associated with the last time a given package was
/// installed?
fn hash_of_last_install(hashes: &Path, pkgbase: &str) -> Result<String, std::io::Error> {
    let path = hashes.join(pkgbase);
    std::fs::read_to_string(path).map(|s| s.trim().to_string())
}

fn overwrite_build_files(
    fll: &FluentLanguageLoader,
    editor: &str,
    build_d: &Path,
    pkgbase: &str,
) -> Result<(), Error> {
    // --- Edit the PKGBUILD in-place --- //
    if proceed!(fll, "A-build-hotedit-pkgbuild").is_some() {
        let pkgbuild = build_d.join("PKGBUILD");
        edit(editor, pkgbuild)?;
    }

    // --- Edit the post-build `.install` file, if there is one --- //
    let install = {
        let mut install = build_d.join(pkgbase);
        install.set_extension("install");
        install
    };

    if install.is_file() && proceed!(fll, "A-build-hotedit-install").is_some() {
        edit(editor, install)?;
    }

    // --- Edit any available .patch files --- //
    build_d
        .read_dir()?
        .filter_map(|de| de.ok())
        .map(|de| de.path())
        .filter(|path| path.extension() == Some("patch".as_ref()))
        .map(|path| edit(editor, path))
        .collect::<Result<(), Error>>()?;

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
                    let target = cache.join(file);
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
    r: Result<Built, Error>,
) -> Result<Option<Built>, Error> {
    match r {
        Ok(tbs) => Ok(Some(tbs)),
        Err(e) => {
            red!(fll, "A-build-fail");
            eprintln!("\n  {}\n", e);
            match proceed!(fll, "continue") {
                Some(_) => Ok(None),
                None => Err(Error::Cancelled),
            }
        }
    }
}
