# Get Caption for Box-and-Whisker Plot

Generates a caption for the box-and-whisker plot based on the data and
configuration.

## Usage

``` r
getCaptionForBoxwhiskerPlot(
  plotDataPk,
  percentiles = NULL,
  yScale = NULL,
  plotCaptionAddon,
  isPlotCaption = TRUE,
  asRatio
)
```

## Arguments

- plotDataPk:

  A data.table containing plot data.

- percentiles:

  A vector of percentiles to calculate.

- yScale:

  Scale type (linear or log).

- plotCaptionAddon:

  Additional text for the caption.

- isPlotCaption:

  Logical indicating if the caption is for the plot.

- asRatio:

  boolean indicating if the plot is for ratios.

## Value

A character string containing the caption.
