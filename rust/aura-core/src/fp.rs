//! Functional Programming utilities.

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
