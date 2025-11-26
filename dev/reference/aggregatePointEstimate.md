# Aggregate Point Estimates

Aggregates point estimates from the filtered PK parameter data based on
the specified ratio mode.

## Usage

``` r
aggregatePointEstimate(
  pkParameterFiltered,
  onePlotConfig,
  ratioMode,
  nBootstrap,
  confLevel,
  aggregationFun,
  aggregationFlag
)
```

## Arguments

- pkParameterFiltered:

  A list of data tables containing filtered PK parameter data.

- onePlotConfig:

  Configuration for the plot.

- ratioMode:

  A character string indicating the mode for ratio calculations.

- nBootstrap:

  Integer specifying the number of bootstrap samples.

- confLevel:

  Numeric value for the confidence level of the intervals.

- aggregationFun:

  Function used for aggregation.

- aggregationFlag:

  Optional aggregation method.

## Value

A data table containing the aggregated point estimates and confidence
intervals.
