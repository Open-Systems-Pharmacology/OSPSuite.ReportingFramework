# ospsuite.reportingframework (development version)

## Bug Fixes

- Fixed issue where file loading fails when LibreOffice converts dashes to hyphens in scenario.xlsx filenames. The `createScenarios.wrapped()` function now automatically detects and corrects hyphen variants (EN DASH U+2013, EM DASH U+2014, etc.) in model file paths (#issue_number).

# ospsuite.reportingframework 1.0.1

- Initial release of the package as beta version
