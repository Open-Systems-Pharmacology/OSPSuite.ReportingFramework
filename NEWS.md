# ospsuite.reportingframework (development version)

## Bug Fixes

- Fixed issue where file loading fails when LibreOffice converts standard hyphens to EN DASHes or other dash variants in scenario.xlsx filenames. The `createScenarios.wrapped()` function now automatically detects and corrects these dash variants (EN DASH U+2013, EM DASH U+2014, etc.) back to standard hyphen-minus in model file paths.

# ospsuite.reportingframework 1.0.1

- Initial release of the package as beta version
