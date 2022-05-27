//! Analyze many aspects of your installation for validity.

use crate::{aura, env::Env, green};
use alpm::Alpm;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::path::Path;

const GOOD: &str = "✓";
const BAD: &str = "✕";

/// Validate the system.
pub(crate) fn check(fll: &FluentLanguageLoader, alpm: &Alpm, env: &Env) {
    aura!(fll, "check-start");
    let caches = env.caches();
    pacman_config(fll, &env.pacman);
    snapshots(fll, &env.backups.snapshots, &caches);
    cache(fll, alpm, &caches);
    green!(fll, "common-done");
}

fn pacman_config(fll: &FluentLanguageLoader, c: &pacmanconf::Config) {
    aura!(fll, "check-conf");
    parallel_downloads(fll, c);
}

fn parallel_downloads(fll: &FluentLanguageLoader, c: &pacmanconf::Config) {
    let good = c.parallel_downloads > 1;
    let symbol = if good { GOOD.green() } else { BAD.red() };
    println!("  [{}] {}", symbol, fl!(fll, "check-conf-parallel"));

    if !good {
        let cmd = "ParallelDownloads".bold().cyan().to_string();
        let msg = fl!(fll, "check-conf-parallel-fix", setting = cmd);
        println!("      └─ {}", msg);
    }
}

fn snapshots(fll: &FluentLanguageLoader, s_path: &Path, t_path: &[&Path]) {
    aura!(fll, "check-snapshots");
    usable_snapshots(fll, s_path, t_path);
}

fn usable_snapshots(fll: &FluentLanguageLoader, s_path: &Path, t_path: &[&Path]) {
    let ss = aura_core::snapshot::snapshots(s_path);
    let vs = aura_core::cache::all_versions(t_path);
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

fn cache(fll: &FluentLanguageLoader, alpm: &Alpm, caches: &[&Path]) {
    aura!(fll, "check-cache");
    caches_exist(fll, caches);
    packages_have_tarballs(fll, alpm, caches);
    valid_tarballs(fll, alpm, caches);
}

fn caches_exist(fll: &FluentLanguageLoader, caches: &[&Path]) {
    let (goods, bads): (Vec<&Path>, _) = caches.iter().partition(|p| p.is_dir());
    let good = bads.is_empty();
    let symbol = if good { GOOD.green() } else { BAD.red() };
    println!(
        "  [{}] {} ({}/{})",
        symbol,
        fl!(fll, "check-cache-exists"),
        goods.len(),
        goods.len() + bads.len()
    );

    if !good {
        for bad in bads {
            println!("      └─ {}", bad.display());
        }
    }
}

/// Is every tarball in the cache valid and loadable by ALPM?
fn valid_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, caches: &[&Path]) {
    let (goods, bads): (Vec<_>, _) = aura_core::cache::package_paths(caches)
        .partition(|pp| aura_arch::is_valid_package(alpm, pp.as_path()));
    let good = bads.is_empty();
    let symbol = if good { GOOD.green() } else { BAD.red() };
    println!(
        "  [{}] {} ({}/{})",
        symbol,
        fl!(fll, "check-cache-tarballs"),
        goods.len(),
        goods.len() + bads.len()
    );

    if !good {
        let cmd = "aura -Ct".bold().cyan().to_string();
        let msg = fl!(fll, "check-cache-tarballs-fix", command = cmd);
        println!("      └─ {}", msg);
    }
}

/// Does every installed package have a tarball in the cache?
fn packages_have_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, caches: &[&Path]) {
    let is_bad = aura_core::cache::missing_tarballs(alpm, caches)
        .next()
        .is_some();
    let symbol = if is_bad { BAD.red() } else { GOOD.green() };
    println!("  [{}] {}", symbol, fl!(fll, "check-cache-missing"));

    if is_bad {
        let cmd = "aura -Cy".bold().cyan().to_string();
        let msg = fl!(fll, "check-cache-missing-fix", command = cmd);
        println!("      └─ {}", msg);
    }
}
