//! Analyze many aspects of your installation for validity.

use crate::env::{Aur, Env};
use crate::error::Nested;
use crate::localization::{identifier_from_code, locale_to_code, Localised};
use crate::utils::PathStr;
use crate::{aura, executable, green};
use alpm::PackageReason;
use colored::*;
use from_variants::FromVariants;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed_fl::fl;
use r2d2::Pool;
use r2d2_alpm::{Alpm, AlpmManager};
use rayon::prelude::*;
use std::collections::HashSet;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

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
    packages(fll, &alpm);
    green!(fll, "common-done");

    Ok(())
}

fn environment(fll: &FluentLanguageLoader) {
    aura!(fll, "check-env");
    lang(fll);
    editor(fll);
    java(fll);
    executable!(fll, "git", "check-env-installed", exec = "git");
    executable!(fll, "fd", "check-env-installed", exec = "fd");
    executable!(fll, "rg", "check-env-installed", exec = "rg");
}

fn java(fll: &FluentLanguageLoader) {
    match which::which("archlinux-java") {
        Err(_) => {
            let msg = fl!(fll, "check-env-java-bin");
            println!("  [{}] {}", WARN.yellow(), msg);
            let pkg = "jdk-openjdk".cyan().to_string();
            let msg = fl!(fll, "check-env-java-bin-fix", pkg = pkg);
            println!("      └─ {}", msg);
        }
        Ok(_) => {
            let good = crate::utils::cmd_lines("archlinux-java", &["status"])
                .map(|lines| lines.last().starts_with("No Java environment").not())
                .unwrap_or(false);
            let symb = if good { GOOD.green() } else { BAD.red() };
            println!("  [{}] {}", symb, fl!(fll, "check-env-java-set"));

            if !good {
                let cmd = "archlinux-java --help".cyan().to_string();
                let msg = fl!(fll, "check-env-java-set-fix", cmd = cmd);
                println!("      └─ {}", msg);
            }
        }
    }
}

fn lang(fll: &FluentLanguageLoader) {
    match std::env::var("LANG") {
        Err(_) => {
            let cmd = "locale -a".cyan().to_string();
            let msg = fl!(fll, "check-env-lang", cmd = cmd, lang = "???");
            println!("  [{}] {}", BAD.red(), msg);
            let msg = fl!(fll, "check-env-lang-fix2");
            println!("      └─ {}", msg);
        }
        Ok(lang) => {
            let good = crate::utils::cmd_lines("locale", &["-a"])
                .map(|lines| lines.into_iter().any(|line| same_lang(&lang, &line)))
                .unwrap_or(false);

            let symb = if good { GOOD.green() } else { BAD.red() };
            let cmd = "locale -a".cyan().to_string();
            let msg = fl!(fll, "check-env-lang", cmd = cmd, lang = lang.clone());
            println!("  [{}] {}", symb, msg);

            if !good {
                let file = "/etc/locale.gen".cyan().to_string();
                let lnge = lang.cyan().to_string();
                let msg = fl!(fll, "check-env-lang-fix", file = file, lang = lnge);
                println!("      └─ {}", msg);
            }

            aura_knows_lang(fll, &lang);
        }
    }
}

fn aura_knows_lang(fll: &FluentLanguageLoader, lang: &str) {
    let good = locale_to_code(lang)
        .and_then(identifier_from_code)
        .is_some();
    let symb = if good { GOOD.green() } else { WARN.yellow() };
    println!("  [{}] {}", symb, fl!(fll, "check-env-lang-known"));
}

/// Whether the LANG variable content can be considered the same as a given line
/// from `locale -a`.
fn same_lang(lang: &str, locale: &str) -> bool {
    match (locale_to_code(lang), locale_to_code(locale)) {
        (Some(l0), Some(l1)) => l0 == l1,
        _ => false,
    }
}

fn editor(fll: &FluentLanguageLoader) {
    let edit = std::env::var("EDITOR");
    let good = edit.is_ok();
    let symb = if good { GOOD.green() } else { WARN.yellow() };
    println!("  [{}] {}", symb, fl!(fll, "check-env-editor"));

    if let Ok(e) = edit.as_deref() {
        let exec = e.cyan().to_string();
        executable!(fll, e, "check-env-editor-exec", exec = exec);
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
    old_aura_dirs(fll);
    old_aura_conf(fll);
}

fn old_aura_dirs(fll: &FluentLanguageLoader) {
    let good = Path::new("/var/cache/aura").is_dir().not();
    let symbol = if good { GOOD.green() } else { WARN.yellow() };
    println!("  [{}] {}", symbol, fl!(fll, "check-aconf-old-dirs"));

    if !good {
        if let Ok(cache) = crate::dirs::aura_xdg_cache() {
            let old = "/var/cache/aura".bold().yellow().to_string();
            let new = cache.display().to_string().bold().cyan().to_string();
            let msg = fl!(fll, "common-replace", old = old, new = new);
            println!("      └─ {}", msg);
        }
    }
}

fn old_aura_conf(fll: &FluentLanguageLoader) {
    if let Ok(xdg) = crate::dirs::xdg_config() {
        let user = xdg.join("aura").join("aura.conf");
        let files = [Path::new("/etc/aura.conf"), &user];
        let exists: Vec<_> = files.into_iter().filter(|p| p.is_file()).collect();
        let good = exists.is_empty();
        let symbol = if good { GOOD.green() } else { WARN.yellow() };
        println!("  [{}] {}", symbol, fl!(fll, "check-aconf-old-conf"));

        if let Ok(aura) = crate::dirs::aura_config() {
            let new = aura.display().to_string().bold().cyan().to_string();

            let len = exists.len();
            for (i, file) in exists.into_iter().enumerate() {
                let old = file.display().to_string().bold().yellow().to_string();
                let msg = fl!(fll, "common-replace", old = old, new = new.as_str());
                let arrow = if i + 1 == len { "└─" } else { "├─" };
                println!("      {} {}", arrow, msg);
            }
        }
    }
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
        let cmd = "aura conf --gen > ~/.config/aura/config.toml"
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
    foreign_packages_have_tarballs(fll, alpm, caches);
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
                .map(|alpm| aura_core::is_valid_package(&*alpm, pp.as_path()))
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
    let all_installed = aura_core::native_packages(alpm).count();
    let bads: Vec<_> = aura_core::cache::officials_missing_tarballs(alpm, caches).collect();
    let is_bad = bads.is_empty().not();
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
}

/// Does every installed foreign package have a tarball in the cache?
fn foreign_packages_have_tarballs(fll: &FluentLanguageLoader, alpm: &Alpm, caches: &[&Path]) {
    let all_installed = aura_core::foreign_packages(alpm).count();
    let bads: Vec<_> = aura_core::cache::foreigns_missing_tarballs(alpm, caches).collect();
    let is_bad = bads.is_empty().not();
    let symbol = if is_bad { BAD.red() } else { GOOD.green() };
    println!(
        "  [{}] {} ({}/{})",
        symbol,
        fl!(fll, "check-cache-missing-for"),
        all_installed - bads.len(),
        all_installed
    );

    if is_bad {
        let cmd = "aura -Cm".bold().cyan().to_string();
        let msg = fl!(fll, "check-cache-missing-for-fix", cmd = cmd);
        println!("      └─ {}", msg);
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

fn packages(fll: &FluentLanguageLoader, alpm: &Alpm) {
    aura!(fll, "check-pkgs");
    old_packages(fll, alpm);
}

fn old_packages(fll: &FluentLanguageLoader, alpm: &Alpm) {
    let now = SystemTime::now();

    if let Ok(dur) = now.duration_since(UNIX_EPOCH) {
        let sec = dur.as_secs();

        let old: Vec<_> = alpm
            .as_ref()
            .localdb()
            .pkgs()
            .into_iter()
            // Only consider packages that you explicitly installed...
            .filter(|p| p.reason() == PackageReason::Explicit)
            // ...and aren't required by anything.
            .filter(|p| p.required_by().is_empty())
            .filter(|p| p.optional_for().is_empty())
            .filter_map(|p| {
                p.install_date().and_then(|id| {
                    let diff = (sec - id as u64) / SECS_IN_DAY;
                    if diff > 365 {
                        Some((p, diff))
                    } else {
                        None
                    }
                })
            })
            .collect();

        let good = old.is_empty();
        let symb = if good { GOOD.green() } else { WARN.yellow() };
        println!("  [{}] {}", symb, fl!(fll, "check-pkgs-old"));

        let len = old.len();
        for (i, (p, diff)) in old.into_iter().enumerate() {
            let pkg = p.name().cyan().to_string();

            let day = if diff < 365 * 2 {
                diff.to_string().yellow().to_string()
            } else {
                diff.to_string().red().to_string()
            };

            let msg = fl!(fll, "check-pkgs-old-warn", pkg = pkg, days = day);
            let arrow = if i + 1 == len { "└─" } else { "├─" };
            println!("      {} {}", arrow, msg);
        }
    }
}
