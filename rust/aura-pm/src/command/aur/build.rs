use crate::aln;
use crate::aura;
use crate::env::Env;
use crate::error::Nested;
use crate::localization::Localised;
use crate::proceed;
use crate::red;
use crate::utils::PathStr;
use crate::utils::ResultVoid;
use crate::yellow;
use aura_core::aur::dependencies::Interdeps;
use aura_core::cache::PkgPath;
use colored::Colorize;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::debug;
use log::error;
use log::warn;
use nonempty_collections::NESet;
use nonempty_collections::NEVec;
use nonempty_collections::NonEmptyIterator;
use r2d2_alpm::Alpm;
use srcinfo::Srcinfo;
use std::collections::HashSet;
use std::ops::Not;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use validated::Validated;

pub(crate) enum Error {
    Srcinfo(PathBuf, srcinfo::Error),
    GitDiff(aura_core::git::Error),
    CopyBuildFiles(NEVec<std::io::Error>),
    Utf8(std::str::Utf8Error),
    FilenameExtraction(PathBuf),
    TarballMove(PathBuf),
    EditFail(PathBuf),
    CreateDir(PathBuf, std::io::Error),
    ReadDir(PathBuf, std::io::Error),
    Pkglist(PathBuf, std::io::Error),
    Makepkg,
    PkgctlBuild,
    Cancelled,
    Permissions(PathBuf),
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
            Error::PkgctlBuild => {}
            Error::Permissions(_) => {}
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
            Error::PkgctlBuild => fl!(fll, "A-build-e-pkgctl"),
            Error::CreateDir(p, _) => fl!(fll, "dir-mkdir", dir = p.utf8()),
            Error::ReadDir(p, _) => fl!(fll, "err-read-dir", dir = p.utf8()),
            Error::Pkglist(p, _) => fl!(fll, "A-build-pkglist", dir = p.utf8()),
            Error::Permissions(p) => fl!(fll, "A-build-e-perm", dir = p.utf8()),
        }
    }
}

/// The results of a successful build.
pub(crate) struct Built {
    pub(crate) clone: PathBuf,
    pub(crate) tarballs: Vec<PkgPath>,
}

// TODO Thu Jan 20 16:13:54 2022
//
// Consider parallel builds, but make it opt-in.
//
// 2024-06-12
//
// Really? Given that certain packages themselves build with multiple threads,
// this sounds like a recipe for problems.
/// Build the given packages and yield paths to their built tarballs.
#[allow(clippy::too_many_arguments)]
pub(crate) fn build<I>(
    fll: &FluentLanguageLoader,
    caches: &[&Path],
    env: &Env,
    alpm: &Alpm,
    editor: &str,
    // Was there only ever one package to be built? If so, we don't prompt the
    // user with a "will you continue?" message if the build fails.
    is_single: bool,
    requested: &HashSet<&str>,
    pkg_clones: I,
) -> Result<Vec<Built>, Error>
where
    I: Iterator<Item = PathBuf>,
{
    aura!(fll, "A-build-prep");

    let to_install = pkg_clones
        .map(|path| build_one(fll, caches, env, alpm, editor, requested, path))
        .map(|r| build_check(fll, env, is_single, r))
        .collect::<Result<Vec<Option<Built>>, Error>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok(to_install)
}

fn build_one(
    fll: &FluentLanguageLoader,
    caches: &[&Path],
    env: &Env,
    alpm: &Alpm,
    editor: &str,
    requested: &HashSet<&str>,
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
    let base = info.base.pkgbase.as_str();

    aura!(fll, "A-build-pkg", pkg = base.cyan().bold().to_string());

    // --- Prepare the Build Directory --- //
    let build_dir = env.aur.build.join(base);
    std::fs::create_dir_all(&build_dir).map_err(|e| Error::CreateDir(build_dir.clone(), e))?;

    // --- Copy non-downloadable `source` files and PKGBUILD --- //
    let to_copy = info
        .base
        .source
        .iter()
        .flat_map(|av| av.vec.iter())
        .filter(|file| (file.contains("https://") || file.contains("http://")).not())
        .map(|s| s.as_str());

    let install_files = all_install_files(&clone);

    std::iter::once("PKGBUILD")
        .chain(install_files.iter().filter_map(|pb| pb.to_str()))
        .chain(to_copy)
        .map(|file| {
            debug!("Copying {}", file);
            let path = Path::new(&file);
            let source = clone.join(path);
            let target = build_dir.join(path);
            std::fs::copy(source, target).void()
        })
        .collect::<Validated<(), std::io::Error>>()
        .ok()
        .map_err(Error::CopyBuildFiles)?;

    if env.aur.diff {
        show_diffs(fll, env, &clone, base)?;
    }

    if env.aur.hotedit {
        overwrite_build_files(fll, env, editor, &build_dir, base)?;
    }

    if env.aur.shellcheck {
        shellcheck(fll, env, &build_dir)?;
    }

    let tarballs = {
        // NOTE 2024-07-23 `pkgctl build` cannot be used as root, as it invokes
        // `makepkg` internally. Nor can it be used by proxy through `nobody`,
        // since the `$HOME` value of `nobody` is `/` and `pkgctl` attempts to
        // `mkdir -p` from there, which it can't. Hacking `HOME=/tmp` also
        // doesn't work, since the user of `pkgbuild build` needs to be a sudoer
        // in order to download packages for the chroot.
        let tarballs = if env.is_root.not() && env.aur.chroot.contains(base) {
            let dbs = alpm.as_ref().syncdbs();
            let aur_deps: Vec<_> = info
                .base
                .makedepends
                .iter()
                .flat_map(|av| av.vec.as_slice())
                .chain(info.pkg.depends.iter().flat_map(|av| av.vec.as_slice()))
                .filter(|s| dbs.find_satisfier(s.as_str()).is_none())
                // `pop` fetches the last item in the Vec, which should be the
                // most recent version of the package.
                .filter_map(|p| aura_core::cache::matching(caches, p).pop())
                .map(|(pp, _)| pp)
                .collect();

            pkgctl_build(&build_dir, &aur_deps)
        } else {
            makepkg(env, &build_dir)
        }?;

        for tb in tarballs.iter() {
            debug!("Built: {}", tb.as_path().display());
        }

        // Filter according to the original packages asked for.
        let interdeps = Interdeps::from_srcinfo(&info);

        let special: Option<NESet<_>> = requested.iter().fold(None, |acc, req| {
            let set = interdeps.transitive(req);

            match (acc, set) {
                (None, None) => None,
                (None, Some(s)) => Some(s),
                (Some(a), None) => Some(a),
                (Some(a), Some(s)) => Some(a.union(&s).map(|x| *x).collect()),
            }
        });

        // FIXME 2024-07-06 I suspect this doesn't account for "debug" packages.
        let tars_to_copy = match special {
            None => tarballs,
            Some(s) => tarballs
                .into_iter()
                .filter(|pp| s.contains(pp.as_package().name.as_ref()))
                .collect(),
        };

        // --- Copy all build artifacts to the cache, and then ignore any sig files --- //
        copy_to_cache(&env.aur.cache, tars_to_copy)?
            .into_iter()
            .filter(|path| {
                path.as_path()
                    .extension()
                    .map(|ex| ex != "sig")
                    .unwrap_or(false)
            })
            .collect::<Vec<_>>()
    };

    if env.aur.clean {
        // NOTE 2024-07-27 As a matter of policy, this call failing should not
        // fail the entire rest of the build process, so we just catch it and
        // warn.
        if let Err(e) = Command::new("rm").arg("-rf").arg(&build_dir).status() {
            warn!("Removing build dir {} failed: {}", build_dir.display(), e);
        }
    }

    Ok(Built { clone, tarballs })
}

/// The PKGBUILD author didn't specify any explicit in the `install` field, but
/// there may be some "install files" lying around anyway. These have
/// inconsistent naming schemes across packages, so we just grab anything that
/// ends with `.install`.
fn all_install_files(clone: &Path) -> Vec<PathBuf> {
    clone
        .read_dir()
        .map(|dir| {
            dir.filter_map(|de| de.ok())
                .map(|de| de.path())
                .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("install"))
                .filter_map(|p| p.file_name().map(|s| s.to_os_string()))
                .map(PathBuf::from)
                .collect()
        })
        .unwrap_or_default()
}

fn show_diffs(
    fll: &FluentLanguageLoader,
    env: &Env,
    clone: &Path,
    pkgbase: &str,
) -> Result<(), Error> {
    let hashes = env.aur.hashes.as_path();

    // Silently skip over any hashes that couldn't be read. This is mostly
    // likely due to the package being installed for the first time, thus having
    // no history to compare to.
    match hash_of_last_install(hashes, pkgbase) {
        Err(e) => warn!("Couldn't read latest hash of {}: {}", pkgbase, e),
        Ok(hash) => {
            if proceed!(fll, env, "A-build-diff").is_some() {
                aura_core::git::diff(clone, &hash).map_err(Error::GitDiff)?;
                proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;
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

fn shellcheck(fll: &FluentLanguageLoader, env: &Env, build_d: &Path) -> Result<(), Error> {
    let status = Command::new("shellcheck")
        .current_dir(build_d)
        .arg("--severity=error")
        .arg("--shell=bash")
        .arg("PKGBUILD")
        .status();

    match status {
        Ok(s) if s.success().not() => {
            proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;
        }
        Err(e) => {
            let msg = e.to_string().yellow();
            aln!(msg);
            proceed!(fll, env, "proceed").ok_or(Error::Cancelled)?;
        }
        Ok(_) => {}
    }

    Ok(())
}

fn overwrite_build_files(
    fll: &FluentLanguageLoader,
    env: &Env,
    editor: &str,
    build_d: &Path,
    pkgbase: &str,
) -> Result<(), Error> {
    // --- Edit the PKGBUILD in-place --- //
    if proceed!(fll, env, "A-build-hotedit-pkgbuild").is_some() {
        let pkgbuild = build_d.join("PKGBUILD");
        edit(editor, pkgbuild)?;
    }

    // --- Edit the post-build `.install` file, if there is one --- //
    let install = {
        let mut install = build_d.join(pkgbase);
        install.set_extension("install");
        install
    };

    if install.is_file() && proceed!(fll, env, "A-build-hotedit-install").is_some() {
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
        .then_some(())
        .ok_or(Error::EditFail(file))?;

    Ok(())
}

fn pkgctl_build(within: &Path, deps: &[PkgPath]) -> Result<Vec<PkgPath>, Error> {
    debug!("Running `pkgctl build` within {}", within.display());
    debug!("AUR deps to inject: {:?}", deps);

    let mut cmd = Command::new("pkgctl");
    cmd.arg("build");

    for dep in deps {
        cmd.arg("-I");
        cmd.arg(dep.as_path());
    }

    cmd.current_dir(within)
        .status()
        .map_err(|_| Error::PkgctlBuild)?
        .success()
        .then_some(())
        .ok_or(Error::PkgctlBuild)?;

    tarball_paths(None, within)
}

/// Build each package specified by the `PKGBUILD` and yield a list of the built
/// tarballs.
fn makepkg(env: &Env, within: &Path) -> Result<Vec<PkgPath>, Error> {
    let user = match env.aur.builduser.as_deref() {
        Some(u) => Some(u),
        // Assumption: The `nobody` user always exists.
        None if env.is_root => Some("nobody"),
        None => None,
    };

    let mut cmd = if let Some(u) = user {
        user_permissions(within, u)?;

        let mut c = Command::new(env.sudo());
        c.arg("-u").arg(u).arg("makepkg");
        c
    } else {
        Command::new("makepkg")
    };

    // TODO Remove or rethink
    //
    // 2024-06-12
    //
    // Yes, this isn't enough to get around packages that don't want to be
    // configured more than once.
    //
    // 2024-07-01
    //
    // The issues is that we _do_ want to leave build artefacts behind in
    // general to speed up rebuilds.
    cmd.arg("-f");

    if env.aur.nocheck {
        cmd.arg("--nocheck");
    }

    if env.aur.skipinteg {
        cmd.arg("--skipinteg");
    }

    if env.aur.skippgpcheck {
        cmd.arg("--skippgpcheck");
    }

    debug!("Running makepkg within: {}", within.display());

    cmd.current_dir(within)
        .status()
        // FIXME Tue Jun 21 14:00:15 2022
        //
        // This should probably collect the error.
        .map_err(|_| Error::Makepkg)?
        .success()
        .then_some(())
        .ok_or(Error::Makepkg)?;

    tarball_paths(user, within)
}

/// Grant write permissions to the given build directory for the given build user.
fn user_permissions(within: &Path, user: &str) -> Result<(), Error> {
    debug!("Setting a+w permissions within: {}", within.display());

    let status = Command::new("chown")
        .arg("-R")
        .arg(user)
        .arg(within)
        .status()
        .map_err(|_| Error::Permissions(within.to_path_buf()))?;

    if status.success() {
        Ok(())
    } else {
        Err(Error::Permissions(within.to_path_buf()))
    }
}

fn tarball_paths(build_user: Option<&str>, within: &Path) -> Result<Vec<PkgPath>, Error> {
    let mut cmd = if let Some(user) = build_user {
        let mut c = Command::new("sudo");
        c.arg("-u").arg(user).arg("makepkg").arg("--packagelist");
        c
    } else {
        let mut c = Command::new("makepkg");
        c.arg("--packagelist");
        c
    };

    // NOTE Outputs absolute paths.
    let bytes = cmd
        .current_dir(within)
        .output()
        .map_err(|e| Error::Pkglist(within.to_path_buf(), e))?
        .stdout;

    let tarballs = std::str::from_utf8(&bytes)
        .map_err(Error::Utf8)?
        .lines()
        .map(PathBuf::from)
        .filter(|path| path.exists())
        .filter_map(PkgPath::new)
        .collect();

    Ok(tarballs)
}

/// Copy each built package tarball (and any signature files) to the package
/// cache, and return a list of the moved files.
fn copy_to_cache(cache: &Path, tarballs: Vec<PkgPath>) -> Result<Vec<PkgPath>, Error> {
    tarballs
        .into_iter()
        .map(|tarball| {
            let path = tarball.into_pathbuf();

            path.file_name()
                .ok_or_else(|| Error::FilenameExtraction(path.clone()))
                .and_then(|file| {
                    let target = cache.join(file);
                    move_tarball(&path, &target).and_then(|_| {
                        PkgPath::new(target.clone())
                            .ok_or_else(|| Error::FilenameExtraction(target))
                    })
                })
        })
        .collect::<Result<Vec<_>, Error>>()
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
        .then_some(())
        .ok_or_else(|| Error::TarballMove(source.to_path_buf()))
}

fn build_check(
    fll: &FluentLanguageLoader,
    env: &Env,
    is_single: bool,
    r: Result<Built, Error>,
) -> Result<Option<Built>, Error> {
    match r {
        Ok(tbs) => Ok(Some(tbs)),
        Err(e) => {
            red!(fll, "A-build-fail");
            eprintln!("\n  {}\n", e.localise(fll));

            if is_single || proceed!(fll, env, "A-build-continue").is_none() {
                Err(Error::Cancelled)
            } else {
                Ok(None)
            }
        }
    }
}
