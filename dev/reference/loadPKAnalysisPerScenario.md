# Load or Calculate PK Analysis for a Given Scenario

This function loads PK analysis results from a CSV file for a specific
scenario.

## Usage

``` r
loadPKAnalysisPerScenario(
  scenarioName,
  scenarioSimulation,
  pkParameterSheets,
  projectConfiguration
)
```

## Arguments

- scenarioName:

  The name of the scenario to be processed.

- scenarioSimulation:

  A simulation object corresponding to the scenario.

- pkParameterSheets:

  A vector of sheet names from the PK parameter file to read.

- projectConfiguration:

  A list containing project configuration settings, including the PK
  parameter file.

## Value

A data.table containing the processed PK analyses for the specified
scenario.
