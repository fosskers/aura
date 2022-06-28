//! Analyze many aspects of your installation for validity.

use crate::env::{Aur, Env};
use crate::error::Nested;
use crate::localization::Localised;
use crate::utils::PathStr;
use crate::{aura, executable, green};
use alpm::Alpm;
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use log::debug;
use r2d2::Pool;
use r2d2_alpm::AlpmManager;
use rayon::prelude::*;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) const GOOD: &str = "✓";
pub(crate) const WARN: &str = "!";
pub(crate) const BAD: &str = "✕";
pub(crate) const CANCEL: &str = "⊘";

const SECS_IN_DAY: u64 = 60 * 60 * 24;

#[derive(FromVariants)]
pub(crate) enum Error {
    Env(crate::env::Error),
}

impl Nested for Error {
    fn nested(&self) {
        match self {
            Error::Env(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::Env(e) => e.localise(fll),
        }
    }
}

/// Validate the system.
pub(crate) fn check(fll: &FluentLanguageLoader, env: &Env) -> Result<(), Error> {
    let caches = env.caches();
    let alpm = env.alpm()?;
    let pool = env.alpm_pool()?;

    aura!(fll, "check-start");
    environment(fll);
    aura_config(fll);
    pacman_config(fll, &env.pacman, &env.aur);
    makepkg_config(fll);
    snapshots(fll, &env.backups.snapshots, &caches);
    cache(fll, &alpm, pool, &caches);
    green!(fll, "common-done");

    Ok(())
}

fn environment(fll: &FluentLanguageLoader) {
    aura!(fll, "check-env");
    editor(fll);
    executable!(fll, "git", "check-env-installed", exec = "git");
    executable!(fll, "fd", "check-env-installed", exec = "fd");
    executable!(fll, "rg", "check-env-installed", exec = "rg");
}

fn editor(fll: &FluentLanguageLoader) {
    let edit = std::env::var("EDITOR");
    let good = edit.is_ok();
    let symb = if good { GOOD.green() } else { WARN.yellow() };
    println!("  [{}] {}", symb, fl!(fll, "check-env-editor"));

    if let Ok(e) = edit.as_deref() {
        executable!(fll, e, "check-env-editor-exec", exec = e);
    } else {
        executable!(fll, "vi", "check-env-editor-vi");
    }
}

fn pacman_config(fll: &FluentLanguageLoader, c: &pacmanconf::Config, a: &Aur) {
    aura!(fll, "check-pconf");
    parallel_downloads(fll, c);
    duplicate_ignores(fll, c, a);
    pacnews(fll);
}

fn makepkg_config(fll: &FluentLanguageLoader) {
    aura!(fll, "check-mconf");
    packager_set(fll);
}

fn aura_config(fll: &FluentLanguageLoader) {
    aura!(fll, "check-aconf");
    parsable_aura_toml(fll);
}

fn parsable_aura_toml(fll: &FluentLanguageLoader) {
    let exists = crate::dirs::aura_config()
        .map(|file| file.is_file())
        .unwrap_or(false);
    let symbol = if exists { GOOD.green() } else { WARN.yellow() };
    println!("  [{}] {}", symbol, fl!(fll, "check-aconf-aura-exists"));

    if exists {
        let parsable = crate::env::parsable_env();
        let symbol = if parsable { GOOD.green() } else { BAD.red() };
        println!("  [{}] {}", symbol, fl!(fll, "check-aconf-aura-parse"));
    } else {
        let cmd = "aura conf --gen > ~/.config/aura.toml"
            .bold()
            .cyan()
            .to_string();
        let msg = fl!(fll, "check-aconf-aura-exists-fix", cmd = cmd);
        println!("      └─ {}", msg);
    }
}

fn packager_set(fll: &FluentLanguageLoader) {
    let (cmd, args) = crate::command::misc::searcher();
    let good = Command::new(cmd)
        .args(args)
        .arg("PACKAGER")
        .arg("/etc/makepkg.conf")
        .output()
        .ok()
        .map(|o| o.stdout)
        .and_then(|stdout| String::from_utf8(stdout).ok())
        .map(|s| s.trim().lines().any(|line| line.starts_with("PACKAGER=")))
        .unwrap_or(false);
    let symbol = if good { GOOD.green() } else { BAD.red() };
    println!("  [{}] {}", symbol, fl!(fll, "check-mconf-packager"));

    if !good {
        let cmd = "PACKAGER=\"You <you@foo.com>\"".cyan().to_string();
        let msg = fl!(fll, "check-mconf-packager-fix", cmd = cmd);
        println!("      └─ {}", msg);
    }
}

fn parallel_downloads(fll: &FluentLanguageLoader, c: &pacmanconf::Config) {
    let good = c.parallel_downloads > 1;
    let symbol = if good { GOOD.green() } else { BAD.red() };
    println!("  [{}] {}", symbol, fl!(fll, "check-pconf-par"));

    if !good {
        let cmd = "ParallelDownloads".bold().cyan().to_string();
        let fix = format!("ParallelDownloads = {}", num_cpus::get())
            .bold()
            .cyan()
            .to_string();
        let msg = fl!(fll, "check-pconf-par-fix", setting = cmd, set = fix);
        println!("      └─ {}", msg);
    }
}

fn duplicate_ignores(fll: &FluentLanguageLoader, c: &pacmanconf::Config, a: &Aur) {
    let pi: HashSet<_> = c.ignore_pkg.iter().map(|s| s.as_str()).collect();
    let ai: HashSet<_> = a.ignores.iter().map(|s| s.as_str()).collect();
    let mut ix = pi.intersection(&ai).copied().collect::<Vec<_>>();

    let good = ix.is_empty();
    let symbol = if good { GOOD.green() } else { BAD.red() };
    println!("  [{}] {}", symbol, fl!(fll, "check-pconf-ignores"));

    if !good {
        ix.sort_unstable();
        let ps = ix.join(", ");
        let msg = fl!(fll, "check-pconf-ignores-fix", pkgs = ps);
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

fn cache(fll: &FluentLanguageLoader, alpm: &Alpm, pool: Pool<AlpmManager>, caches: &[&Path]) {
    aura!(fll, "check-cache");
    caches_exist(fll, caches);
    official_packages_have_tarballs(fll, alpm, caches);
    valid_tarballs(fll, pool, caches);
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
fn valid_tarballs(fll: &FluentLanguageLoader, pool: Pool<AlpmManager>, caches: &[&Path]) {
    let (goods, bads): (Vec<_>, Vec<_>) = aura_core::cache::package_paths(caches)
        .par_bridge()
        .partition(|pp| {
            pool.get()
                .map(|alpm| aura_arch::is_valid_package(&alpm, pp.as_path()))
                .unwrap_or(false)
        });
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
fn official_packages_have_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, caches: &[&Path]) {
    let all_installed = aura_arch::officials(alpm).count();
    let bads: Vec<_> = aura_core::cache::officials_missing_tarballs(alpm, caches).collect();
    let is_bad = bads.len() > 0;
    let symbol = if is_bad { BAD.red() } else { GOOD.green() };
    println!(
        "  [{}] {} ({}/{})",
        symbol,
        fl!(fll, "check-cache-missing"),
        all_installed - bads.len(),
        all_installed
    );

    if is_bad {
        let cmd = "aura -Cy".bold().cyan().to_string();
        let msg = fl!(fll, "check-cache-missing-fix", command = cmd);
        println!("      └─ {}", msg);
    }

    #[cfg(debug_assertions)]
    for bad in bads {
        debug!("{}", bad.name());
    }
}

fn pacnews(fll: &FluentLanguageLoader) {
    match which::which("fd") {
        Err(_) => {
            println!(
                "  [{}] {}",
                CANCEL.truecolor(128, 128, 128),
                fl!(fll, "check-pconf-pacnew")
            );
            println!(
                "      └─ {}",
                fl!(fll, "check-missing-exec", exec = "fd".cyan().to_string())
            );
        }
        Ok(_) => match pacnew_work() {
            None => {
                println!(
                    "  [{}] {}",
                    CANCEL.truecolor(128, 128, 128),
                    fl!(fll, "check-pconf-pacnew")
                );
                println!(
                    "      └─ {}",
                    fl!(
                        fll,
                        "check-pconf-pacnew-broken",
                        fd = "fd".cyan().to_string()
                    )
                );
            }
            Some(bads) => {
                let good = bads.is_empty();
                let sym = if good { GOOD.green() } else { BAD.red() };
                println!("  [{}] {}", sym, fl!(fll, "check-pconf-pacnew"));

                let len = bads.len();
                for (i, (path, days)) in bads.into_iter().enumerate() {
                    let arrow = if i + 1 == len { "└─" } else { "├─" };

                    println!(
                        "      {} {}",
                        arrow,
                        fl!(
                            fll,
                            "check-pconf-pacnew-old",
                            path = path.utf8().cyan().to_string(),
                            days = days.to_string().red().to_string(),
                        ),
                    );
                }
            }
        },
    }
}

/// Attempt to produce a list of paths for which the current in-use config file
/// is older than its associated `.pacnew`. For each such path, also include how
/// many days out-of-date it is.
fn pacnew_work() -> Option<Vec<(PathBuf, u64)>> {
    let outp = Command::new("fd").args([".pacnew", "/etc"]).output().ok()?;
    let stdo = std::str::from_utf8(&outp.stdout).ok()?;
    let bads = stdo
        .trim()
        .lines()
        .map(Path::new)
        .filter(|p| p.extension() == Some("pacnew".as_ref()))
        .map(|new| (new.with_extension(""), new))
        .filter_map(|(orig, new)| {
            orig.metadata()
                .ok()
                .and_then(|o_m| new.metadata().ok().map(|n_m| (orig, o_m, n_m)))
        })
        .filter_map(|(orig, o_m, n_m)| {
            o_m.modified()
                .ok()
                .and_then(|o_mod| n_m.modified().ok().map(|n_mod| (orig, o_mod, n_mod)))
        })
        .filter(|(_, o_m, n_m)| o_m < n_m)
        .filter_map(|(orig, o_m, n_m)| {
            n_m.duration_since(o_m)
                .ok()
                .map(|d| (orig, d.as_secs() / SECS_IN_DAY))
        })
        .collect();

    Some(bads)
}
