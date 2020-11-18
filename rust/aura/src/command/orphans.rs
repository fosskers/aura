use arch::Alpm;
use aura_arch as arch;

/// Print the name of each orphaned package.
pub fn list(alpm: &Alpm) {
    arch::orphans(&alpm)
        .iter()
        .for_each(|o| println!("{}", o.name()))
}
