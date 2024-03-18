# r2d2-alpm

## 0.2.0 (2023-03-19)

#### Added

- A wrapper `Alpm` struct around `alpm::Alpm`. This allows `ManageConnection` to
  be implemented again, thus supporting Version 3 of `alpm`.
  
#### Changed

- `ManageConnection::Connection` now yields the new wrapper type.
