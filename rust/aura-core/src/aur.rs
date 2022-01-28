//! Core interactions with the AUR.

pub mod dependencies;

use log::debug;
use raur_curl::Raur;
use std::borrow::Cow;
use std::path::Path;

/// The base path of the URL.
pub const AUR_BASE_URL: &str = "https://aur.archlinux.org/";

/// AUR package information.
pub fn info<S>(packages: &[S]) -> Result<Vec<raur_curl::Package>, raur_curl::Error>
where
    S: AsRef<str>,
{
    raur_curl::Handle::new().info(packages)
}

/// Search the AUR for packages matching a query.
pub fn search(query: &str) -> Result<Vec<raur_curl::Package>, raur_curl::Error> {
    raur_curl::Handle::new().search(query)
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
pub fn partition_aur_pkgs<'a, S>(
    clone_dir: &Path,
    packages: &'a [S],
) -> Result<PkgPartition<'a>, raur_curl::Error>
where
    S: AsRef<str>,
{
    let (cloned, fast_bads): (Vec<Cow<'a, str>>, Vec<Cow<'a, str>>) = packages
        .iter()
        .map(|p| Cow::Borrowed(p.as_ref()))
        .partition(|p| is_aur_package_fast(clone_dir, p));
    let mut part = partition_real_pkgs_via_aur(clone_dir, fast_bads)?;

    part.cloned.extend(cloned);
    Ok(part)
}

/// Quickly check some given package's name against the local cache of package
/// clones to see if its a real AUR package.
///
/// This of course isn't fool proof, since it doesn't consult the AUR, and thus
/// the caller should follow up with an AUR call if this returns `false`.
fn is_aur_package_fast(clone_dir: &Path, package: &str) -> bool {
    let mut path = clone_dir.to_path_buf();
    path.push(package);
    path.is_dir()
}

// TODO Tue Jan 18 20:13:12 2022
//
// If this is ever made `pub`, switch it to a generic `S: AsRef<str>`.
/// Performs an AUR `info` call to determine which packages are real or not.
fn partition_real_pkgs_via_aur<'a>(
    clone_dir: &Path,
    packages: Vec<Cow<'a, str>>,
) -> Result<PkgPartition<'a>, raur_curl::Error> {
    debug!("AUR call to check for: {:?}", packages);

    let info: Vec<_> = info(&packages)?;

    // First we need to determine which of the AUR-returned packages were
    // actually "split" packages (i.e. those built as a child of another, like
    // gcc6-libs).
    let (originals, splits): (Vec<_>, _) = info.into_iter().partition(|p| p.package_base == p.name);

    // If there were some splits, some of their parents might already have
    // clones, so we need to recheck those.
    let (pkgbase_cloned, pkgbase_to_clone): (Vec<_>, _) = splits
        .into_iter()
        .partition(|p| is_aur_package_fast(clone_dir, &p.package_base));

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
