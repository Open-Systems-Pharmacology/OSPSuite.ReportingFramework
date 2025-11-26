# Load PK Analysis Results from CSV

This function loads PK analysis results from a CSV file or recalculates
them if the file is not found. It returns the loaded data as a
data.table.

## Usage

``` r
loadPkAnalysisRawData(projectConfiguration, scenarioName, scenarioSimulation)
```

## Arguments

- projectConfiguration:

  A list containing project configuration settings, including the output
  folder path and the PK parameter file.

- scenarioName:

  The name of the scenario for which PK analysis is to be loaded.

- scenarioSimulation:

  A simulation object corresponding to the scenario.

## Value

A data.table containing the PK analyses loaded from the CSV file.
