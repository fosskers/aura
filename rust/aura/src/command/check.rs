//! Analyze many aspects of your installation for validity.

use crate::{aln, aura, green};
use alpm::Alpm;
use colored::*;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use std::path::Path;

const GOOD: &str = "✓";
const BAD: &str = "✕";

/// Validate the system.
pub(crate) fn check(fll: &FluentLanguageLoader, alpm: &Alpm, cache_path: &Path) {
    aura!(fll, "check-start");
    cache(fll, alpm, cache_path);
    green!(fll, "common-done");
}

fn cache(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) {
    aura!(fll, "check-cache");
    valid_tarballs(fll, alpm, cache);
}

/// Is every tarball in the cache valid and loadable by ALPM?
fn valid_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, cache: &Path) {
    match aura_core::cache::package_paths(cache) {
        Err(_) => {
            let p = cache.to_str().unwrap();
            let msg = fl!(fll, "check-cache-unreadable", path = p);
            println!("  [{}] {}", "✕".red(), msg);
        }
        Ok(paths) => {
            // We short-circuit if even a single invalid tarball is found.
            // This keeps the operation fast.
            let is_bad = paths
                .filter(|pp| !aura_arch::is_valid_package(alpm, pp.path()))
                .next()
                .is_some();

            let symbol = if !is_bad { GOOD.green() } else { BAD.red() };
            println!("  [{}] {}", symbol, fl!(fll, "check-cache-tarballs"));

            if is_bad {
                let msg = fl!(
                    fll,
                    "check-cache-tarballs-fix",
                    command = "aura -Ct".bold().cyan().to_string()
                );
                println!("      └─ {}", msg);
            }
        }
    }
}
