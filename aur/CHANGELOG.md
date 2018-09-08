# Changelog

## 6.1.0

- RPC functions now return in `Maybe`, for occasions where connection to the AUR
  was impossible. Previously, that case would result in an empty list.
- RPC functions now return in a monomorphized `IO`, instead of the previous
  polymorphic `MonadIO`.
