# Prepare Data for PK Forest

Prepares data for generating a pharmacokinetic (PK) forest plot.

## Usage

``` r
prepareDataForPKForest(
  onePlotConfig,
  pkParameterFiltered,
  dataObservedPK,
  ratioMode,
  asPointeEstimate,
  nBootstrap,
  confLevel,
  aggregationFun,
  aggregationFlag
)
```

## Arguments

- onePlotConfig:

  A configuration list for the plot.

- pkParameterFiltered:

  A data table containing filtered PK parameter data.

- dataObservedPK:

  An optional data table for observed PK parameters.

- ratioMode:

  A string indicating the mode for ratio calculations.

- asPointeEstimate:

  A logical indicating if point estimates should be calculated.

- nBootstrap:

  An integer specifying the number of bootstrap samples.

- confLevel:

  A numeric value representing the confidence level for intervals.

- aggregationFun:

  A function used for data aggregation.

- aggregationFlag:

  An optional string indicating the aggregation method.

## Value

A list of prepared data for the PK forest plot.
