//! Core interactions with the AUR.

pub mod dependencies;

use log::debug;
use std::borrow::Cow;
use std::path::Path;
use std::path::PathBuf;

/// The base path of the URL.
pub const AUR_BASE_URL: &str = "https://aur.archlinux.org/";

/// Errors in handling AUR packages.
pub enum Error {
    /// Some problem involving pulling or cloning.
    Git(crate::git::Error),
    /// Calling the Faur failed somehow.
    FaurFetch(String),
    /// A package exists in no way on the AUR.
    PackageDoesNotExist(String),
    /// Somehow the Faur returned too many results.
    TooManyFaurResults(String),
}

impl From<crate::git::Error> for Error {
    fn from(v: crate::git::Error) -> Self {
        Self::Git(v)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Git(e) => write!(f, "{e}"),
            Error::FaurFetch(p) => write!(f, "Calling the metadata server utterly failed: {p}"),
            Error::PackageDoesNotExist(p) => write!(f, "Unknown package: {p}"),
            Error::TooManyFaurResults(p) => {
                write!(f, "More results returned from Faur than expected: {p}")
            }
        }
    }
}

/// The result of inspecting the existance status of a collection of package
/// names.
pub struct PkgPartition<'a> {
    /// These packages already have local clones.
    pub cloned: Vec<Cow<'a, str>>,
    /// These packages still need to be cloned.
    pub to_clone: Vec<Cow<'a, str>>,
    /// These packages don't exist at all!
    pub not_real: Vec<Cow<'a, str>>,
}

/// Given a [`Path`] to an expected directory of AUR package repo clones, check
/// it and the AUR to determine which of a given collection of packages are
/// actually real.
pub fn partition_aur_pkgs<'a, S, F, E>(
    fetch: &F,
    clone_d: &Path,
    packages: &'a [S],
) -> Result<PkgPartition<'a>, E>
where
    S: AsRef<str>,
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E>,
{
    let (cloned, fast_bads): (Vec<Cow<'a, str>>, Vec<Cow<'a, str>>) = packages
        .iter()
        .map(|p| Cow::Borrowed(p.as_ref()))
        .partition(|p| has_local_aur_clone(clone_d, p));
    let mut part = partition_real_pkgs_via_aur(fetch, clone_d, fast_bads)?;

    part.cloned.extend(cloned);
    Ok(part)
}

/// Quickly check some given package's name against the local cache of package
/// clones to see if its a real AUR package.
///
/// This of course isn't fool proof, since it doesn't consult the AUR, and thus
/// the caller should follow up with an AUR call if this returns `false`.
pub fn has_local_aur_clone(clone_d: &Path, pkg: &str) -> bool {
    clone_d.join(pkg).is_dir()
}

// TODO Tue Jan 18 20:13:12 2022
//
// If this is ever made `pub`, switch it to a generic `S: AsRef<str>`.
/// Performs an AUR `info` call to determine which packages are real or not.
fn partition_real_pkgs_via_aur<'a, F, E>(
    fetch: &F,
    clone_d: &Path,
    packages: Vec<Cow<'a, str>>,
) -> Result<PkgPartition<'a>, E>
where
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E>,
{
    debug!("AUR call to check for: {:?}", packages);

    let info: Vec<_> = crate::faur::info(packages.iter().map(|cow| cow.as_ref()), fetch)?;

    // First we need to determine which of the AUR-returned packages were
    // actually "split" packages (i.e. those built as a child of another, like
    // gcc6-libs).
    let (originals, splits): (Vec<_>, _) = info.into_iter().partition(|p| p.package_base == p.name);

    // If there were some splits, some of their parents might already have
    // clones, so we need to recheck those.
    let (pkgbase_cloned, pkgbase_to_clone): (Vec<_>, _) = splits
        .into_iter()
        .partition(|p| has_local_aur_clone(clone_d, &p.package_base));

    // Anything else must not really exist.
    let not_real = packages
        .into_iter()
        .filter(|pn| {
            let s = pn.as_ref();
            originals.iter().all(|p| p.name != s)
                && pkgbase_cloned.iter().all(|p| p.name != s)
                && pkgbase_to_clone.iter().all(|p| p.name != s)
        })
        .collect();

    let cloned = pkgbase_cloned
        .into_iter()
        .map(|p| Cow::Owned(p.package_base))
        .collect();

    let to_clone = originals
        .into_iter()
        .chain(pkgbase_to_clone)
        .map(|p| Cow::Owned(p.package_base))
        .collect();

    let part = PkgPartition {
        cloned,
        to_clone,
        not_real,
    };

    Ok(part)
}

/// Clone a package's AUR repository and return the full path to the clone.
pub fn clone_aur_repo(root: Option<&Path>, package: &str) -> Result<PathBuf, crate::git::Error> {
    let mut url: PathBuf = [AUR_BASE_URL, package].iter().collect();
    url.set_extension("git");

    let clone_path: PathBuf = match root {
        None => PathBuf::from(package),
        Some(r) => r.join(package),
    };

    crate::git::shallow_clone(&url, &clone_path).map(|_| clone_path)
}

/// Yield a path to the local git clone of the given package. The path won't
/// exactly match the original name of the package when it wasn't the original
/// `pkgbase` (example: gcc6-libs -> gcc6).
///
/// Either way, if there was no local clone present, this will cause a `git
/// clone` to occur.
pub fn clone_path_of_pkgbase<F, E>(clone_d: &Path, pkg: &str, fetch: &F) -> Result<PathBuf, Error>
where
    F: Fn(&str) -> Result<Vec<crate::faur::Package>, E>,
{
    let path: PathBuf = if has_local_aur_clone(clone_d, pkg) {
        clone_d.join(pkg)
    } else {
        let ps = crate::faur::info([pkg], fetch).map_err(|_| Error::FaurFetch(pkg.to_string()))?;
        let fp = match ps.as_slice() {
            [fp] => Ok(fp),
            [] => Err(Error::PackageDoesNotExist(pkg.to_string())),
            [_, _, ..] => Err(Error::TooManyFaurResults(pkg.to_string())),
        }?;

        if has_local_aur_clone(clone_d, &fp.package_base) {
            clone_d.join(&fp.package_base)
        } else {
            clone_aur_repo(Some(clone_d), &fp.package_base)?
        }
    };

    Ok(path)
}
