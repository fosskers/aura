//! Specification of a consistent system environment that you can easily port to
//! other machines. It allows you to specify packages that you always want
//! installed, as well as automatically managed symlinks to various config
//! files.
//!
//! Obviously this is not as fully featured as Guix or Nix, but it covers the
//! basic functionality often sought from tools like Guix Home. At the same
//! time, this formalizes a basic package setup that people often write ad hoc
//! scripts for, and thus rests somewhere between the two poles.
