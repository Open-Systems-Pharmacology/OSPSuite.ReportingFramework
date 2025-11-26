# Prepare Data for PK Boxplot

Prepares and cleans data for box-and-whisker plotting by merging the PK
parameter data with the configuration settings. This function ensures
that the data is structured correctly for plotting.

## Usage

``` r
prepareTableForExport(dtExport, asRatio, plotCaptionAddon, plotDataPk)
```

## Arguments

- dtExport:

  A data.table containing the data to be exported for plotting.

- asRatio:

  A logical indicating if the plot is for ratios.

- plotCaptionAddon:

  An optional string to be added to the plot caption.

- plotDataPk:

  A data.table containing PK parameter data for generating the plot
  caption.

## Value

A data.table prepared for plotting, including merged configuration and
parameter data.
