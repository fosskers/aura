# r2d2-alpm

Types and utilities for the spawing of a `r2d2::Pool` that handles multiple
connections to an Arch Linux `Alpm` database.

## Usage

To create a `Pool` that delegates `Alpm` connections within (for instance) some
Rayon threads:

```rust
use r2d2::Pool;
use r2d2_alpm::AlpmManager;
use rayon::prelude::*;

let mngr = AlpmManager::from_file("/etc/pacman.conf").unwrap();
let pool = Pool::builder().max_size(4).build(mngr).unwrap();

(0..10).into_par_iter().for_each_with(pool, |p, n| {
    // `Pool::get` will wait for a configurable length
    // of time for a free connection before giving up.
    if let Ok(alpm) = p.get() {
        // Use the ALPM handle freely here.
    }
});
```

Like `std::sync::Arc`, `Pool` is cheap to `Clone`, making it ideal for a number
of Rayon methods like `map_with`, etc.
