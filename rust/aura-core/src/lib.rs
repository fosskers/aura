//! Core package manager functionality that doesn't assume a certain frontend,
//! logging framework, or Error stack.

#![warn(missing_docs)]

pub mod cache;
pub mod common;
pub mod deps;
pub mod fp;
pub mod git;
pub mod log;
pub mod snapshot;
pub mod sums;
