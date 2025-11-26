# Add Summary Table from Plot Data

Generates a summary table from the box-and-whisker plot data.

## Usage

``` r
getSummaryTable(plotDataPk, asRatio, onePlotConfig, percentiles)
```

## Arguments

- plotDataPk:

  A data.table containing plot data.

- asRatio:

  boolean indicating if the plot is for ratios.

- onePlotConfig:

  Configuration for a single plot.

- percentiles:

  A vector of percentiles to calculate.

## Value

A data.table summarizing the plot data.
