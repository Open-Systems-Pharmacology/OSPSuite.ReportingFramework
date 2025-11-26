# Prepare Data for PK Boxplot

Prepares and cleans data for box-and-whisker plotting.

## Usage

``` r
prepareDataForPKBoxplot(onePlotConfig, pkParameterDT, colorVector, asRatio)
```

## Arguments

- onePlotConfig:

  Configuration for a single plot.

- pkParameterDT:

  A data.table containing PK parameter data.

- colorVector:

  A named vector for colors.

- asRatio:

  boolean indicating if the plot is for ratios.

## Value

A data.table prepared for plotting.
