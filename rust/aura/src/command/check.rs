//! Analyze many aspects of your installation for validity.

use crate::{aura, green};
use alpm::Alpm;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::path::Path;

const GOOD: &str = "✓";
const BAD: &str = "✕";

/// Validate the system.
pub(crate) fn check(
    fll: &FluentLanguageLoader,
    alpm: &Alpm,
    cache_path: &Path,
    snapshot_path: &Path,
) {
    aura!(fll, "check-start");
    // directories(fll, snapshot_path);
    snapshots(fll, snapshot_path, cache_path);
    cache(fll, alpm, cache_path);
    green!(fll, "common-done");
}

// fn directories(fll: &FluentLanguageLoader, s_path: &Path) {
//     aura!(fll, "check-directories");
//     directory_existance(fll, s_path);
// }

fn snapshots(fll: &FluentLanguageLoader, s_path: &Path, t_path: &Path) {
    aura!(fll, "check-snapshots");
    usable_snapshots(fll, s_path, t_path);
}

fn usable_snapshots(fll: &FluentLanguageLoader, s_path: &Path, t_path: &Path) {
    match aura_core::snapshot::snapshots(s_path) {
        Err(_) => unreadable_snapshots(fll, s_path),
        Ok(ss) => match aura_core::cache::all_versions(t_path) {
            Err(_) => unreadable_cache(fll, t_path),
            Ok(vs) => {
                let (goods, bads): (Vec<_>, Vec<_>) = ss.partition(|s| s.usable(&vs));
                let good = bads.is_empty();

                let symbol = if good { GOOD.green() } else { BAD.red() };
                println!(
                    "  [{}] {} ({}/{})",
                    symbol,
                    fl!(fll, "check-snapshot-usable"),
                    goods.len(),
                    goods.len() + bads.len()
                );

                if !good {
                    let cmd = "aura -Bc".bold().cyan().to_string();
                    let msg = fl!(fll, "check-snapshot-usable-fix", command = cmd);
                    println!("      └─ {}", msg);
                }
            }
        },
    }
}

fn cache(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) {
    aura!(fll, "check-cache");
    packages_have_tarballs(fll, alpm, cache);
    valid_tarballs(fll, alpm, cache);
}

/// Is every tarball in the cache valid and loadable by ALPM?
fn valid_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) {
    match aura_core::cache::package_paths(cache) {
        Err(_) => unreadable_cache(fll, cache),
        Ok(mut paths) => {
            let good = paths.all(|pp| aura_arch::is_valid_package(alpm, pp.path()));

            let symbol = if good { GOOD.green() } else { BAD.red() };
            println!("  [{}] {}", symbol, fl!(fll, "check-cache-tarballs"));

            if !good {
                let cmd = "aura -Ct".bold().cyan().to_string();
                let msg = fl!(fll, "check-cache-tarballs-fix", command = cmd);
                println!("      └─ {}", msg);
            }
        }
    }
}

/// Does every installed package have a tarball in the cache?
fn packages_have_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) {
    match aura_core::cache::missing_tarballs(alpm, cache) {
        Err(_) => unreadable_cache(fll, cache),
        Ok(mut pkgs) => {
            let is_bad = pkgs.next().is_some();

            let symbol = if is_bad { BAD.red() } else { GOOD.green() };
            println!("  [{}] {}", symbol, fl!(fll, "check-cache-missing"));

            if is_bad {
                let cmd = "aura -Cy".bold().cyan().to_string();
                let msg = fl!(fll, "check-cache-missing-fix", command = cmd);
                println!("      └─ {}", msg);
            }
        }
    }
}

fn unreadable_cache(fll: &FluentLanguageLoader, cache: &Path) {
    let p = cache.to_str().unwrap();
    let msg = fl!(fll, "check-cache-unreadable", path = p);
    println!("  [{}] {}", "✕".red(), msg);
}

fn unreadable_snapshots(fll: &FluentLanguageLoader, path: &Path) {
    let p = path.to_str().unwrap();
    let msg = fl!(fll, "check-snapshots-unreadable", path = p);
    println!("  [{}] {}", "✕".red(), msg);
}
