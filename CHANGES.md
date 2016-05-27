# 0.2.0 (unreleased)

- Use thyme instead of time. Thyme uses a much more efficient representation of
  time stamps. Note that thyme has a module called Data.Thyme.Time that
  provides wrappers and conversion functions to and from time types.
- Added a convenient `granularityToDiffTime` to convert from `Granularity` to
  `NominalDiffTime`.

# 0.1.0.0

Initial release. The API is not yet complete, but there is enough to be useful.
