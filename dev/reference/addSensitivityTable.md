# Add Sensitivity Table to Project Configuration

This function adds a sensitivity table to the provided project
configuration. It loads a template Excel file and populates it with
parameter paths from the specified scenario.

## Usage

``` r
addSensitivityTable(
  projectConfiguration,
  scenarioList = NULL,
  scenarioName,
  sheetName = scenarioName
)
```

## Arguments

- projectConfiguration:

  An object representing the project configuration.

- scenarioList:

  A list of scenarios from which to extract parameter paths. Defaults to
  NULL.

- scenarioName:

  The name of the scenario to extract parameter paths from.

- sheetName:

  The name of the sheet in the Excel file to populate. Defaults to the
  value of scenarioName.

## Value

The modified project configuration object.
