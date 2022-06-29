use aura::flags::*;
use clap::{Command, CommandFactory};
use clap_mangen::Man;
use std::fs::File;
use std::path::Path;

const DIR: &str = "man-pages";

fn main() -> std::io::Result<()> {
    [
        (Args::command(), "aura.1"),
        (Database::command(), "aura-database.1"),
        (Files::command(), "aura-files.1"),
        (Query::command(), "aura-query.1"),
        (Remove::command(), "aura-remove.1"),
        (Sync::command(), "aura-sync.1"),
        (DepTest::command(), "aura-dep-test.1"),
        (Upgrade::command(), "aura-upgrade.1"),
        (Aur::command(), "aura-aur.1"),
        (Backup::command(), "aura-backup.1"),
        (Cache::command(), "aura-cache.1"),
        (Log::command(), "aura-log.1"),
        (Orphans::command(), "aura-orphans.1"),
        (Conf::command(), "aura-conf.1"),
        (Stats::command(), "aura-stats.1"),
        (Open::command(), "aura-open.1"),
        (Deps::command(), "aura-deps.1"),
        (Check::command(), "aura-check.1"),
    ]
    .into_iter()
    .try_for_each(|(cmd, path)| work(cmd, path))
}

/// Render and output the manpage for some given Clap [`Command`].
fn work<P>(cmd: Command<'_>, file: P) -> std::io::Result<()>
where
    P: AsRef<Path>,
{
    let man = Man::new(cmd);
    let mut path = File::create(Path::new(DIR).join(file))?;
    man.render(&mut path)
}
