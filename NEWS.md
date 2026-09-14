# ospsuite.reportingframework (development version)

- Added a new `DATACLASS` value `numericValues` to import scalar numeric values from configuration sheets.
- Extended `readObservedDataByDictionary()` with `dataClassType = "numericValues"`.
- Added support for `NumericValues` sheets with columns `VariableName`, `Value`, `Unit`, `Reference`, and `Description` (`Description` is metadata only and is not returned).
- Added `getNumericValue()` helper to retrieve one typed scalar value with optional unit checking.

# ospsuite.reportingframework 1.0.1

- Initial release of the package as beta version
