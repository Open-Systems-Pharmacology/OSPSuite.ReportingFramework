# Create a Box and Whisker Plot Object

This helper function generates a box and whisker plot object based on
the provided plot data and configuration. It supports different ratio
modes and applies the appropriate aesthetics and scales to the plot.

## Usage

``` r
createBaseBoxWhisker(
  plotDataPk,
  yScale,
  asRatio,
  colorVector,
  onePlotConfig,
  ...
)
```

## Arguments

- plotDataPk:

  A data frame containing the plot data for a specific PK parameter.

- yScale:

  A character string indicating the scale for the y-axis (e.g.,
  "linear", "log").

- asRatio:

  boolean indicating if values shoud be evaluated as ratio or absolute
  values.

- colorVector:

  A vector of colors to be used for filling the plot.

- onePlotConfig:

  A list containing configuration settings for the plot.

- ...:

  Additional arguments to be passed to the plotting functions.

## Value

A ggplot object representing the box and whisker plot.
