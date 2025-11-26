# Merge Data Groups into Configuration

This internal function merges data group information into the
configuration table based on the provided scenarios and observed data.

## Usage

``` r
mergeDataGroups(dtNewConfig, dtDataGroups, dataObserved)
```

## Arguments

- dtNewConfig:

  A data.table containing the new configuration for time profile plots.

- dtDataGroups:

  A data.table containing data group information.

- dataObserved:

  Optional. A data object containing observed data, if available.

## Value

A data.table with merged data group information.
