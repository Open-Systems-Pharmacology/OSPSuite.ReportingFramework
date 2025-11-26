# Get Caption for Demographic Plot

Generates a caption for demographic plots based on the provided data.

## Usage

``` r
getCaptionForDemographicPlot(
  idData,
  valueLabel,
  binLabel,
  valueScale,
  plotCaptionAddon
)
```

## Arguments

- idData:

  A data.table containing the plot data.

- valueLabel:

  A label for the value axis.

- binLabel:

  A label for the bin axis (optional).

- valueScale:

  A scale type for the value axis (optional).

- plotCaptionAddon:

  Additional text to append to the caption (optional).

## Value

A string containing the generated caption for the plot.
