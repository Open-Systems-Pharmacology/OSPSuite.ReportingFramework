# Filter Observed Parameters

Filters observed PK parameters based on the provided configuration.

## Usage

``` r
filterParameterObserved(dataObservedPK, onePlotConfig)
```

## Arguments

- dataObservedPK:

  Optional data.table containing observed PK parameter data for
  comparison, which can include columns for observed values and
  associated metadata.

- onePlotConfig:

  A data.table containing configuration settings for a single plot,
  including plot name, x-axis scale, and other aesthetic settings.

## Value

Filtered data table of observed PK parameters.
