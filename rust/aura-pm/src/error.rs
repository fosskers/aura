//! All errors that can occur in the Aura executable.

use crate::localization::Localised;
use aura_core::aur::dependencies as deps;
use i18n_embed::fluent::FluentLanguageLoader;
use log::error;

/// Error type for all issues that can occur in the Aura library or executable.
pub(crate) enum Error {
    A(crate::command::aur::Error),
    B(crate::command::snapshot::Error),
    C(crate::command::cache::Error),
    L(crate::command::logs::Error),
    O(crate::command::orphans::Error),
    /// A non-zero exit code was returned from a call to Pacman.
    Pacman(crate::pacman::Error),
    Env(crate::env::Error),
    Conf(crate::conf::Error),
    Check(crate::check::Error),
    Open(crate::open::Error),
    Stats(crate::stats::Error),
    Deps(crate::deps::Error),
}

impl From<crate::deps::Error> for Error {
    fn from(v: crate::deps::Error) -> Self {
        Self::Deps(v)
    }
}

impl From<crate::open::Error> for Error {
    fn from(v: crate::open::Error) -> Self {
        Self::Open(v)
    }
}

impl From<crate::stats::Error> for Error {
    fn from(v: crate::stats::Error) -> Self {
        Self::Stats(v)
    }
}

impl From<crate::conf::Error> for Error {
    fn from(v: crate::conf::Error) -> Self {
        Self::Conf(v)
    }
}

impl From<crate::command::orphans::Error> for Error {
    fn from(v: crate::command::orphans::Error) -> Self {
        Self::O(v)
    }
}

impl From<crate::command::logs::Error> for Error {
    fn from(v: crate::command::logs::Error) -> Self {
        Self::L(v)
    }
}

impl From<crate::command::cache::Error> for Error {
    fn from(v: crate::command::cache::Error) -> Self {
        Self::C(v)
    }
}

impl From<crate::command::snapshot::Error> for Error {
    fn from(v: crate::command::snapshot::Error) -> Self {
        Self::B(v)
    }
}

impl From<crate::env::Error> for Error {
    fn from(v: crate::env::Error) -> Self {
        Self::Env(v)
    }
}

impl From<crate::command::aur::Error> for Error {
    fn from(v: crate::command::aur::Error) -> Self {
        Self::A(v)
    }
}

impl From<crate::pacman::Error> for Error {
    fn from(v: crate::pacman::Error) -> Self {
        Self::Pacman(v)
    }
}

impl Nested for Error {
    /// Log nested errors.
    fn nested(&self) {
        match self {
            Error::A(e) => e.nested(),
            Error::B(e) => e.nested(),
            Error::C(e) => e.nested(),
            Error::L(e) => e.nested(),
            Error::O(e) => e.nested(),
            Error::Pacman(e) => e.nested(),
            Error::Env(e) => e.nested(),
            Error::Conf(e) => e.nested(),
            Error::Check(e) => e.nested(),
            Error::Open(e) => e.nested(),
            Error::Stats(e) => e.nested(),
            Error::Deps(e) => e.nested(),
        }
    }
}

impl Localised for Error {
    fn localise(&self, fll: &FluentLanguageLoader) -> String {
        match self {
            Error::A(e) => e.localise(fll),
            Error::B(e) => e.localise(fll),
            Error::C(e) => e.localise(fll),
            Error::L(e) => e.localise(fll),
            Error::O(e) => e.localise(fll),
            Error::Pacman(e) => e.localise(fll),
            Error::Env(e) => e.localise(fll),
            Error::Conf(e) => e.localise(fll),
            Error::Check(e) => e.localise(fll),
            Error::Open(e) => e.localise(fll),
            Error::Stats(e) => e.localise(fll),
            Error::Deps(e) => e.localise(fll),
        }
    }
}

/// Do something with the nested errors of this type.
///
/// Quite weak as far as typeclasses go; it's entirely lawless. It's entire
/// purpose is to be able to render errors out of lib crates that have no access
/// to the error handling facilities of the application.
pub(crate) trait Nested {
    fn nested(&self);
}

impl Nested for aura_core::git::Error {
    fn nested(&self) {
        match self {
            aura_core::git::Error::Io(e) => error!("{e}"),
            aura_core::git::Error::Clone(_) => {}
            aura_core::git::Error::Pull(_) => {}
            aura_core::git::Error::Diff(_) => {}
            aura_core::git::Error::ReadHash(e) => error!("{e}"),
        }
    }
}

impl Nested for aura_core::aur::Error {
    fn nested(&self) {
        match self {
            aura_core::aur::Error::Git(e) => e.nested(),
            aura_core::aur::Error::FaurFetch(_) => {}
            aura_core::aur::Error::PackageDoesNotExist(_) => {}
            aura_core::aur::Error::TooManyFaurResults(_) => {}
        }
    }
}

impl<E> Nested for deps::Error<E>
where
    E: Nested,
{
    fn nested(&self) {
        match self {
            deps::Error::PoisonedMutex => {}
            deps::Error::R2D2(e) => error!("{e}"),
            deps::Error::Srcinfo(_, e) => error!("{e}"),
            deps::Error::Git(e) => e.nested(),
            deps::Error::Resolutions(es) => es.iter().into_iter().for_each(|e| e.nested()),
            deps::Error::DoesntExist(_) => {}
            deps::Error::DoesntExistWithParent(_, _) => {}
            deps::Error::MalformedGraph => {}
            deps::Error::CyclicDep(_) => {}
            deps::Error::Faur(e) => e.nested(),
        }
    }
}
