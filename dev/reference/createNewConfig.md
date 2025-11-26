# Create New Configuration for Time Profile Plots

This internal function generates a new configuration data table for time
profile plots based on the provided scenarios and project configuration.

## Usage

``` r
createNewConfig(scenarios, dataObserved)
```

## Arguments

- scenarios:

  A data.table containing scenario definitions.

- dataObserved:

  Optional. A data object containing observed data, if available.

## Value

A data.table containing the new configuration for time profile plots.
