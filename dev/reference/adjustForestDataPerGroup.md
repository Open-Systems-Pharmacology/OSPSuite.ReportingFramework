# Adjust Forest Data Per Group

Processes a data group to ensure unique scenarios and generates plot
tags.

## Usage

``` r
adjustForestDataPerGroup(dataGroup, onePlotConfig)
```

## Arguments

- dataGroup:

  A data table representing a group of plot data.

- onePlotConfig:

  Configuration for the plot, containing metadata like plot names.

## Value

A modified data table with updated \`outputPathId\` as a factor and
generated \`plotTag\`.

## Details

This function checks the uniqueness of scenarios within the provided
data group, modifies the \`outputPathId\` to be a factor with specified
levels, and generates plot tags based on the \`outputPathId\`.
