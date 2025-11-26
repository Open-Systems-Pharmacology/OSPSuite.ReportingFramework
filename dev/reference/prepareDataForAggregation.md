# Prepares data for aggregation

Prepares data for aggregation

## Usage

``` r
prepareDataForAggregation(dataObserved, groups, groupSuffix)
```

## Arguments

- dataObserved:

  A \`data.table\` containing observed data.

- groups:

  A character vector specifying the groups to aggregate. If NULL, all
  available groups are used.

- groupSuffix:

  A character string to append to group names in the aggregated output.
  Default is 'aggregated'.

## Value

Prepared data for aggregation as a \`data.table\`.
