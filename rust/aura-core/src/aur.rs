//! Core interactions with the AUR.

use crate::common::{Bad, Good};
use log::debug;
use raur_curl::Raur;
use std::collections::HashSet;
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

/// Given a [`Path`] to an expected directory of AUR package repo clones, check
/// it and the AUR to determine which of a given collection of packages are
/// actually real.
pub fn partition_aur_pkgs<'a, S>(
    clone_dir: &Path,
    packages: &'a [S],
) -> Result<(Good<Vec<&'a str>>, Bad<Vec<&'a str>>), raur_curl::Error>
where
    S: AsRef<str>,
{
    let (mut goods, fast_bads): (Vec<&'a str>, Vec<&'a str>) = packages
        .iter()
        .map(|p| p.as_ref())
        .partition(|p| is_aur_package_fast(clone_dir, p));
    let (slow_goods, bads) = partition_real_pkgs_via_aur(&fast_bads)?;
    goods.extend(slow_goods.good);

    Ok((Good::new(goods), bads))
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

/// Performs an AUR `info` call to determine which packages are real or not.
fn partition_real_pkgs_via_aur<'a>(
    packages: &[&'a str],
) -> Result<(Good<Vec<&'a str>>, Bad<Vec<&'a str>>), raur_curl::Error> {
    debug!("AUR call to check for: {:?}", packages);

    let info = info(packages)?;
    let reals: HashSet<&str> = info.iter().map(|p| p.name.as_str()).collect();
    let (goods, bads): (Vec<_>, Vec<_>) = packages.iter().partition(|p| reals.contains(*p));

    Ok((Good::new(goods), Bad::new(bads)))
}
