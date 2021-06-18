//! Functional Programming utilities.

use std::iter::FromIterator;

/// Any type that is fundamentally combinable.
pub trait Semigroup {
    /// A total combination of two elements of a categorical `Semigroup`.
    ///
    /// In other words, add two things together!
    fn add(self, other: Self) -> Self;
}

impl Semigroup for () {
    fn add(self, _: ()) -> () {
        ()
    }
}

/// All `Semigroup`s which also have a notion of some "unit" element.
pub trait Monoid: Semigroup {
    /// The "unit" element of this Monoidal category.
    fn empty() -> Self;
}

impl Monoid for () {
    fn empty() -> () {
        ()
    }
}

/// Similar to `Result`, but cumulative in its error type.
///
/// Consider that when using `collect` in a "Traversable" way to pull a single
/// `Result` out of an `Iterator` containing many `Result`s, it will fail on the
/// first `Err` and short-circuit the iteration. This is suboptimal if we wish
/// to be made aware of every failure that (would have) occurred.
pub enum Validated<T, E> {
    /// Analogous to `Result::Ok`.
    Success(T),
    /// Analogous to `Result::Err`, except that the error type is cumulative.
    Failure(Vec<E>),
}

impl<T, E> Semigroup for Validated<T, E>
where
    T: Semigroup,
{
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Validated::Success(a), Validated::Success(b)) => Validated::Success(a.add(b)),
            (Validated::Success(_), e @ Validated::Failure(_)) => e,
            (e @ Validated::Failure(_), Validated::Success(_)) => e,
            (Validated::Failure(mut a), Validated::Failure(b)) => {
                a.extend(b);
                Validated::Failure(a)
            }
        }
    }
}

impl<T, E> FromIterator<Validated<T, E>> for Validated<T, E>
where
    T: Monoid,
{
    fn from_iter<I: IntoIterator<Item = Validated<T, E>>>(iter: I) -> Validated<T, E> {
        let z = Validated::Success(Monoid::empty());
        iter.into_iter().fold(z, |acc, v| acc.add(v))
    }
}

impl<T, E> FromIterator<Result<T, E>> for Validated<T, E>
where
    T: Monoid,
{
    fn from_iter<I: IntoIterator<Item = Result<T, E>>>(iter: I) -> Self {
        let z = Validated::Success(Monoid::empty());
        iter.into_iter().fold(z, |acc, v| match (acc, v) {
            (Validated::Success(a), Ok(b)) => Validated::Success(a.add(b)),
            (Validated::Success(_), Err(e)) => Validated::Failure(vec![e]),
            (e @ Validated::Failure(_), Ok(_)) => e,
            (Validated::Failure(mut a), Err(b)) => {
                a.push(b);
                Validated::Failure(a)
            }
        })
    }
}
