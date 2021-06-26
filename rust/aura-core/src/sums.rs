//! Sum Types.

/// The same as `Either`, a kind of generalized `Result`.
pub enum Two<A, B> {
    /// The first possibility.
    A(A),
    /// The second possibility.
    B(B),
}

/// Like `Either` but with three types.
pub enum Three<A, B, C> {
    /// The first possibility.
    A(A),
    /// The second possibility.
    B(B),
    /// The third possibility.
    C(C),
}
