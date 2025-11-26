# Generate Range Plots

Generates range plots based on the provided configuration and data.

## Usage

``` r
generateRangePlots(
  onePlotConfig,
  plotData,
  colorVector,
  facetAspectRatio,
  asStepPlot,
  aggregationFun,
  ...
)
```

## Arguments

- onePlotConfig:

  A configuration for the specific plot.

- plotData:

  A data.table containing the plot data.

- colorVector:

  A named vector for colors corresponding to scenarios.

- facetAspectRatio:

  A numeric value for the aspect ratio of the facets.

- asStepPlot:

  Logical indicating if the plot should be a step plot.

- aggregationFun:

  A function for aggregating the data for plotting.

- ...:

  Additional arguments passed to the range plotting function.

## Value

A list of range plot objects.
