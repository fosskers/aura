//! The Aura Package Manager.
//!
//! Copyright 2012 - 2024 Colin Woodbury <colin@fosskers.ca>
//!
//! This file is part of Aura.
//!
//! Aura is free s
//!
//!              oftwar
//!         e:youcanredist
//!      ributeitand/ormodify
//!     itunderthetermsoftheGN
//!    UGeneralPublicLicenseasp
//!   ublishedbytheFreeSoftw
//!  areFoundation,either     ver        sio        n3o        fth
//!  eLicense,or(atyou       ropti      on)an      ylate      rvers
//! ion.Auraisdistr         ibutedi    nthehop    ethatit    willbeu
//!  seful,butWITHOUTA       NYWAR      RANTY      ;with      outev
//!  entheimpliedwarranty     ofM        ERC        HAN        TAB
//!   ILITYorFITNESSFORAPART
//!    ICULARPURPOSE.SeetheGNUG
//!     eneralPublicLicensefor
//!      moredetails.Youshoul
//!         dhavereceiveda
//!              copyof
//!
//! the GNU General Public License
//! along with Aura.  If not, see <http://www.gnu.org/licenses/>.

#![warn(missing_docs)]

pub(crate) mod command;
pub(crate) mod dirs;
pub(crate) mod download;
pub(crate) mod env;
pub(crate) mod error;
pub(crate) mod fetch;
pub(crate) mod localization;
mod macros;
pub(crate) mod makepkg;
pub(crate) mod pacman;
pub(crate) mod utils;

use crate::command::aur;
use crate::command::aur::Mode;
use crate::command::cache;
use crate::command::check;
use crate::command::conf;
use crate::command::deps;
use crate::command::logs;
use crate::command::open;
use crate::command::orphans;
use crate::command::snapshot;
use crate::command::stats;
use crate::command::thanks;
use crate::error::Error;
use crate::error::Nested;
use crate::localization::Localised;
use aura_pm::flags::Args;
use aura_pm::flags::Cache;
use aura_pm::flags::SubCmd;
use aura_pm::flags::AURA_GLOBALS;
use aura_pm::ENGLISH;
use clap::Parser;
use colored::Colorize;
use command::free;
use env::Env;
use i18n_embed::fluent::FluentLanguageLoader;
use i18n_embed::LanguageLoader;
use log::debug;
use log::info;
use simplelog::ColorChoice;
use simplelog::Config;
use simplelog::TermLogger;
use simplelog::TerminalMode;
use std::ops::Not;
use std::process::ExitCode;

fn main() -> ExitCode {
    // Parse all CLI input. Exits immediately if invalid input is given.
    let args = aura_pm::flags::Args::parse();

    // --- Terminal Logging --- //
    if let Some(l) = args.log_level {
        // Silently ignore logger init failure. Realistically it should never
        // fail, since its docs claim this only occurs when a logger has been
        // previously initialized.
        let _ = TermLogger::init(l, Config::default(), TerminalMode::Mixed, ColorChoice::Auto);
    }

    debug!("{:#?}", args);

    // --- Runtime Settings --- //
    match env(&args) {
        Err(e) => {
            match localization::load(Some(ENGLISH)) {
                Err(e1) => {
                    aln!("Aura utterly failed to establish a runtime environment.".red());
                    println!("\n  {}", e1);
                }
                Ok(fll) => {
                    aln!(e.localise(&fll).red());
                }
            }

            ExitCode::FAILURE
        }
        Ok(env) => {
            // debug!("{:#?}", env);

            // --- Localisation --- //
            match localization::load(Some(env.general.language.clone())) {
                Err(e) => {
                    aln!("Runtime environment loaded, but failed to localise Aura!".red());
                    println!("\n  {}", e);
                    ExitCode::FAILURE
                }
                Ok(fll) => match work(args, env, &fll) {
                    Err(e) => {
                        e.nested();
                        aln!(e.localise(&fll).red());
                        ExitCode::FAILURE
                    }
                    Ok(_) => ExitCode::SUCCESS,
                },
            }
        }
    }
}

/// Initialize the runtime environment.
fn env(args: &Args) -> Result<Env, Error> {
    let mut env = crate::env::Env::try_new()?;
    env.reconcile_cli(args);
    env.validate()?;
    Ok(env)
}

fn work(args: Args, env: Env, fll: &FluentLanguageLoader) -> Result<(), Error> {
    info!("Language: {}", fll.current_language().language.as_str());

    match args.subcmd {
        // --- Pacman Commands --- //
        SubCmd::Database(d) => pacman(&env, d.needs_sudo())?,
        SubCmd::Files(f) => pacman(&env, f.needs_sudo())?,
        SubCmd::Query(_) => pacman(&env, false)?,
        SubCmd::Remove(r) => pacman(&env, r.needs_sudo())?,
        SubCmd::Sync(s) => pacman(&env, s.needs_sudo())?,
        SubCmd::DepTest(_) => pacman(&env, false)?,
        SubCmd::Upgrade(u) => pacman(&env, u.needs_sudo())?,
        // --- AUR Packages --- //
        SubCmd::Aur(a) if a.info.is_empty().not() => aur::info(fll, &a.info)?,
        SubCmd::Aur(a) if a.search.is_empty().not() => {
            aur::search(&env.alpm()?, a.abc, a.reverse, a.limit, a.quiet, a.search)?
        }
        SubCmd::Aur(a) if a.provides.is_some() => aur::provides(
            &env.alpm()?,
            a.abc,
            a.reverse,
            a.limit,
            a.quiet,
            a.provides.unwrap(),
        )?,
        SubCmd::Aur(a) if a.open.is_some() => aur::open(&a.open.unwrap())?,
        SubCmd::Aur(a) if a.pkgbuild.is_some() => {
            aur::pkgbuild(&a.pkgbuild.unwrap(), &env.aur.clones)?
        }
        SubCmd::Aur(a) if a.wclone.is_empty().not() => aur::clone_aur_repos(fll, &a.wclone)?,
        SubCmd::Aur(a) if a.sysupgrade => aur::upgrade(fll, &env.alpm()?, env, a.dryrun)?,
        SubCmd::Aur(a) if a.refresh => aur::refresh(fll, &env.aur.clones)?,
        SubCmd::Aur(a) => aur::install(
            fll,
            &env,
            Mode::Install,
            a.packages.iter().map(|s| s.as_str()),
        )?,
        // --- Package Sets --- //
        SubCmd::Backup(b) if b.clean => {
            snapshot::clean(fll, &env.caches(), &env.backups.snapshots)?
        }
        SubCmd::Backup(b) if b.list => snapshot::list(&env.backups.snapshots)?,
        SubCmd::Backup(b) if b.restore => snapshot::restore(&env, fll, &env.alpm()?)?,
        SubCmd::Backup(_) => snapshot::save(fll, &env.alpm()?, &env.backups.snapshots)?,
        // --- Cache Management --- //
        SubCmd::Cache(c) if !c.info.is_empty() => {
            cache::info(fll, &env.alpm()?, &env.caches(), c.info)?
        }
        SubCmd::Cache(c) if c.search.is_some() => cache::search(&env.caches(), &c.search.unwrap())?,
        SubCmd::Cache(c) if c.backup.is_some() => cache::backup(fll, &env, &c.backup.unwrap())?,
        SubCmd::Cache(Cache { clean: Some(n), .. }) => cache::clean(&env, fll, n)?,
        SubCmd::Cache(c) if c.clean_unsaved => cache::clean_not_saved(fll, &env)?,
        SubCmd::Cache(c) if c.invalid => cache::invalid(&env, fll, &env.alpm()?, &env.caches())?,
        SubCmd::Cache(c) if c.list => cache::list(&env.caches())?,
        SubCmd::Cache(c) if c.refresh => cache::refresh(&env, fll, &env.alpm()?)?,
        SubCmd::Cache(c) if c.missing => cache::missing(&env.alpm()?, &env.caches()),
        SubCmd::Cache(c) => cache::downgrade(&env, fll, c.packages)?,
        // --- Logs --- //
        SubCmd::Log(l) if l.search.is_some() => logs::search(env.alpm_log(), l.search.unwrap())?,
        SubCmd::Log(l) if !l.info.is_empty() => logs::info(fll, env.alpm_log(), l.info)?,
        SubCmd::Log(l) => logs::view(env.alpm_log(), l.before, l.after)?,
        // --- Orphan Packages --- //
        SubCmd::Orphans(o) if o.abandon => orphans::remove(&env, &env.alpm()?, fll)?,
        SubCmd::Orphans(o) if !o.adopt.is_empty() => {
            orphans::adopt(&env, &env.alpm()?, fll, o.adopt)?
        }
        SubCmd::Orphans(o) if o.elderly => orphans::elderly(&env.alpm()?),
        SubCmd::Orphans(_) => orphans::list(&env.alpm()?),
        // --- PKGBUILD Analysis --- //
        // SubCmd::Analysis(_) => unimplemented!(),
        // --- Configuration --- //
        SubCmd::Conf(c) if c.pacman => conf::open_pacman_conf(c)?,
        SubCmd::Conf(c) if c.aura => conf::open_aura_conf()?,
        SubCmd::Conf(c) if c.makepkg => conf::open_makepkg_conf()?,
        SubCmd::Conf(c) if c.gen => conf::gen(&env)?,
        SubCmd::Conf(_) => conf::general(&env),
        // --- Statistics --- //
        SubCmd::Stats(s) if s.lang => stats::localization()?,
        SubCmd::Stats(s) if s.heavy => stats::heavy_packages(&env.alpm()?),
        SubCmd::Stats(s) if s.groups => stats::groups(&env.alpm()?),
        SubCmd::Stats(_) => stats::stats(&env, fll)?,
        // --- Opening Webpages --- //
        SubCmd::Open(o) if o.docs => open::book()?,
        SubCmd::Open(o) if o.repo => open::repo()?,
        SubCmd::Open(o) if o.bug => open::bug()?,
        SubCmd::Open(o) if o.aur => open::aur()?,
        SubCmd::Open(o) if o.license => open::license()?,
        SubCmd::Open(_) => open::repo()?,
        // --- Dependency Management --- //
        SubCmd::Deps(d) if d.reverse => {
            deps::reverse(&env.alpm()?, d.limit, d.optional, d.raw, d.open, d.packages)?
        }
        SubCmd::Deps(d) => {
            deps::graph(&env.alpm()?, d.limit, d.optional, d.raw, d.open, d.packages)?
        }
        // --- System Validation --- //
        SubCmd::Check(_) => check::check(fll, &env).map_err(Error::Check)?,
        // --- Credits --- //
        SubCmd::Thanks => thanks::thanks(fll),
        // --- Free Software --- //
        SubCmd::Free(f) if f.copyleft => free::copyleft(&env.alpm()?, f.lenient),
        SubCmd::Free(f) => free::free(&env.alpm()?, f.lenient),
    }

    Ok(())
}

/// Run a Pacman command.
fn pacman(env: &Env, sudo: bool) -> Result<(), crate::pacman::Error> {
    let mut raws: Vec<String> = std::env::args()
        .skip(1)
        .filter(|a| !(AURA_GLOBALS.contains(&a.as_str()) || a.starts_with("--log-level=")))
        .collect();

    // Special consideration for split cases like `--log-level debug`.
    if let Some(ix) = raws
        .iter()
        .enumerate()
        .find_map(|(i, v)| (v == "--log-level").then_some(i))
    {
        raws.remove(ix); // --log-level
        raws.remove(ix); // Its argument.
    }

    debug!("Passing to Pacman: {:?}", raws);
    if sudo {
        pacman::sudo_pacman_batch(env, raws)
    } else {
        pacman::pacman(raws)
    }
}
