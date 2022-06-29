use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::{PathStr, ResultVoid};
use crate::{aura, proceed, red, yellow};
use colored::Colorize;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::{debug, error, warn};
use nonempty::NonEmpty;
use srcinfo::Srcinfo;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::process::Command;
use validated::Validated;

pub(crate) enum Error {
    Srcinfo(PathBuf, srcinfo::Error),
    GitDiff(aura_core::git::Error),
    CopyBuildFiles(NonEmpty<std::io::Error>),
    Utf8(std::str::Utf8Error),
    FilenameExtraction(PathBuf),
    TarballMove(PathBuf),
    EditFail(PathBuf),
    CreateDir(PathBuf, std::io::Error),
    ReadDir(PathBuf, std::io::Error),
    Pkglist(PathBuf, std::io::Error),
    Makepkg,
    Cancelled,
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Srcinfo(_, e) => error!("{e}"),
            Error::GitDiff(e) => e.nested(),
            Error::CopyBuildFiles(es) => {
                for e in es {
                    error!("{e}");
                }
            }
            Error::Utf8(e) => error!("{e}"),
            Error::FilenameExtraction(_) => {}
            Error::TarballMove(_) => {}
            Error::EditFail(_) => {}
            Error::CreateDir(_, e) => error!("{e}"),
            Error::ReadDir(_, e) => error!("{e}"),
            Error::Pkglist(_, e) => error!("{e}"),
            Error::Makepkg => {}
            Error::Cancelled => {}
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Srcinfo(p, _) => fl!(fll, "err-srcinfo", file = p.utf8()),
            Error::GitDiff(e) => e.localise(fll),
            Error::CopyBuildFiles(_) => fl!(fll, "A-build-e-copies"),
            Error::Utf8(_) => fl!(fll, "err-utf8"),
            Error::FilenameExtraction(p) => fl!(fll, "A-build-e-filename", file = p.utf8()),
            Error::TarballMove(p) => fl!(fll, "A-build-e-tarball", file = p.utf8()),
            Error::Cancelled => fl!(fll, "common-cancelled"),
            Error::EditFail(p) => fl!(fll, "A-build-e-edit", file = p.utf8()),
            Error::Makepkg => fl!(fll, "A-build-e-makepkg"),
            Error::CreateDir(p, _) => fl!(fll, "dir-mkdir", dir = p.utf8()),
            Error::ReadDir(p, _) => fl!(fll, "err-read-dir", dir = p.utf8()),
            Error::Pkglist(p, _) => fl!(fll, "A-build-pkglist", dir = p.utf8()),
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
    aur: &crate::env::Aur,
    editor: &str,
    is_single: bool,
    pkg_clones: I,
) -> Result<Vec<Built>, Error>
where
    I: Iterator<Item = PathBuf>,
{
    aura!(fll, "A-build-prep");

    let to_install = pkg_clones
        .map(|path| build_one(fll, aur, editor, path))
        .map(|r| build_check(fll, is_single, r))
        .collect::<Result<Vec<Option<Built>>, Error>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok(to_install)
}

fn build_one(
    fll: &FluentLanguageLoader,
    aur: &crate::env::Aur,
    editor: &str,
    clone: PathBuf,
) -> Result<Built, Error> {
    // Attempt a quick `git pull` to avoid the issue of building stale versions
    // of a package if the user forgot to `-Ay` recently.
    if let Err(e) = aura_core::git::pull(&clone) {
        warn!("{e}");
        yellow!(fll, "A-build-pull");
    }

    // --- Parse the .SRCINFO for metadata --- //
    let path = clone.join(".SRCINFO");
    let info = Srcinfo::parse_file(&path).map_err(|e| Error::Srcinfo(path, e))?;
    let base = info.base.pkgbase;

    aura!(fll, "A-build-pkg", pkg = base.cyan().bold().to_string());

    // --- Prepare the Build Directory --- //
    let build = aur.build.join(&base);
    std::fs::create_dir_all(&build).map_err(|e| Error::CreateDir(build.clone(), e))?;

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
        .map_err(Error::CopyBuildFiles)?;

    if aur.diff {
        show_diffs(fll, &aur.hashes, &clone, &base)?;
    }

    if aur.hotedit {
        overwrite_build_files(fll, editor, &build, &base)?;
    }

    let tarballs = {
        let tarballs = makepkg(&build)?;

        for tb in tarballs.iter() {
            debug!("Built: {}", tb.display());
        }

        // --- Copy all build artifacts to the cache, and then ignore any sig files --- //
        copy_to_cache(&aur.cache, &tarballs)?
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
                aura_core::git::diff(clone, &hash).map_err(Error::GitDiff)?;
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
        .read_dir()
        .map_err(|e| Error::ReadDir(build_d.to_path_buf(), e))?
        .filter_map(|de| de.ok())
        .map(|de| de.path())
        .filter(|path| path.extension() == Some("patch".as_ref()))
        .try_for_each(|path| edit(editor, path))?;

    Ok(())
}

fn edit(editor: &str, file: PathBuf) -> Result<(), Error> {
    Command::new(editor)
        .arg(&file)
        .status()
        .map_err(|_| Error::EditFail(file.clone()))?
        .success()
        .then(|| ())
        .ok_or(Error::EditFail(file))?;

    Ok(())
}

/// Build each package specified by the `PKGBUILD` and yield a list of the built
/// tarballs.
fn makepkg(within: &Path) -> Result<Vec<PathBuf>, Error> {
    Command::new("makepkg")
        .arg("-f") // TODO Remove or rethink
        .current_dir(within)
        .status()
        // FIXME Tue Jun 21 14:00:15 2022
        //
        // This should probably collect the error.
        .map_err(|_| Error::Makepkg)?
        .success()
        .then(|| ())
        .ok_or(Error::Makepkg)?;

    // NOTE Outputs absolute paths.
    let bytes = Command::new("makepkg")
        .arg("--packagelist")
        .current_dir(within)
        .output()
        .map_err(|e| Error::Pkglist(within.to_path_buf(), e))?
        .stdout;

    let tarballs = std::str::from_utf8(&bytes)
        .map_err(Error::Utf8)?
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
        .status()
        // FIXME Tue Jun 21 14:04:58 2022
        //
        // The error should probably be collected here too.
        .map_err(|_| Error::TarballMove(source.to_path_buf()))?
        .success()
        .then(|| ())
        .ok_or_else(|| Error::TarballMove(source.to_path_buf()))
}

fn build_check(
    fll: &FluentLanguageLoader,
    is_single: bool,
    r: Result<Built, Error>,
) -> Result<Option<Built>, Error> {
    match r {
        Ok(tbs) => Ok(Some(tbs)),
        Err(e) => {
            red!(fll, "A-build-fail");
            eprintln!("\n  {}\n", e.localise(fll));

            if is_single || proceed!(fll, "A-build-continue").is_none() {
                Err(Error::Cancelled)
            } else {
                Ok(None)
            }
        }
    }
}
