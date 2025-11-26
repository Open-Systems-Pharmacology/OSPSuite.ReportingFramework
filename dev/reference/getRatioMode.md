# Get Ratio Mode for Plot Configuration

This function determines the ratio mode for a given plot configuration
based on the provided parameters. It checks if the scenarios in the plot
configuration have the same or different base populations.

## Usage

``` r
getRatioMode(onePlotConfig, pkParameterDT, asRatio)
```

## Arguments

- onePlotConfig:

  A data frame containing the plot configuration with columns
  'plotName', 'scenario', and 'referenceScenario'.

- pkParameterDT:

  A data frame containing parameter details, including 'scenario' and
  'populationId'.

- asRatio:

  A logical value indicating whether to calculate the ratio mode. If
  FALSE, the function returns 'none'.

## Value

A character string indicating the ratio mode. Possible values are: -
'none' if \`asRatio\` is FALSE. - 'individualRatios' if all population
IDs match between scenarios. - 'ratioOfPopulation' if all population IDs
are different between scenarios.
