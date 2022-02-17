//! Extra utilities.

// FIXME Mon Feb 14 21:46:15 2022
//
// Make this its own crate.

/// Consume ownership.
pub(crate) trait Void {
    fn void(self);
}

impl Void for bool {
    fn void(self) {}
}
