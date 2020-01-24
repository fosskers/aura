# Changelog

## 6.3.0

- `servant-0.17` support.

## 6.2.0.1

- Relax `http-client` bounds.

## 6.2.0

- `info` and `search` now return in `Either`.
- Everything has been combined into a single module.
- Hard requirement on `servant ^>= 0.16`.

## 6.1.0.1

- Massaged various bounds for future-proofing.

## 6.1.0

- RPC functions now return in `Maybe`, for occasions where connection to the AUR
  was impossible. Previously, that case would result in an empty list.
- RPC functions now return in a monomorphized `IO`, instead of the previous
  polymorphic `MonadIO`.
