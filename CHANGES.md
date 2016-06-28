# 0.2.0.0 (unreleased)

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

# 0.1.0.0

Initial release. The API is not yet complete, but there is enough to be useful.
