# Prepare Demographic Plot Data

Prepares the data for demographic plotting based on the provided
configuration and parameters.

## Usage

``` r
prepareDemographicPlotData(
  onePlotConfig,
  pkParameterDT,
  scenarioList,
  usePKParameter,
  asRangePlot,
  colorVector
)
```

## Arguments

- onePlotConfig:

  A configuration for the specific plot.

- pkParameterDT:

  A data.table containing PK parameter data (optional).

- scenarioList:

  A list of scenarios to consider (optional).

- usePKParameter:

  Logical indicating if PK parameters are to be used.

- asRangePlot:

  Logical indicating if the plot should be a range plot.

- colorVector:

  A named vector for colors corresponding to scenarios.

## Value

A data.table containing the prepared data for plotting.
