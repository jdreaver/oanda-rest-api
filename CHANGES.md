# Changelog


## 0.5.0

- Use `time` instead of `thyme`. `time` is a lot faster now, and `thyme` seems
  unmaintained.

## 0.4.1

- Fix style suggestions for newer hlint

## 0.4.0

- Migrated to the new v20 REST API

## 0.3.1

- Added endpoint to create orders

## 0.3.0.0

- Prefer `Text` instead of `String` for arguments. We still use `String` in
  endpoints since that is what `wreq` uses.
- Use `http-conduit` instead of `wreq`

## 0.2.0.0

- Use thyme instead of time. Thyme uses a much more efficient representation of
  time stamps. Note that thyme has a module called Data.Thyme.Time that
  provides wrappers and conversion functions to and from time types.
- Added a convenient `granularityToDiffTime` to convert from `Granularity` to
  `NominalDiffTime`.
- Fixed not being able to use a start/end time in conjunction with a count for
  the candlestick endpoints.
- Use true optional arguments using `Maybe`. This fixes some endpoints breaking
  when empty lists were passed, and also makes it so we don't have to hard-code
  defaults.

## 0.1.0.0

Initial release. The API is not yet complete, but there is enough to be useful.
