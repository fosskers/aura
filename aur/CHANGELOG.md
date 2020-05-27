# Changelog

## 7.0.3 (2020-05-27)

- Bumped `http-client` bounds.

## 7.0.2 (2020-05-26)

#### Changed

- Bumped `aeson` bounds.

## 7.0.1 (2020-05-11)

#### Changed

- Bumped `tasty` bounds.

## 7.0.0 (2020-04-22)

#### Added

- The `AurError` type to account for the API change described below.

#### Changed

- The `servant` dependency has been dropped in favour of vanilla `http-client`.
- `info` and `search` now return the custom `AurError` type instead of servant's
  `ClientError`.

#### Removed

- The reexport of `ClientError` from servant.

## 6.3.1

- Rewiden `servant` bounds.

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
