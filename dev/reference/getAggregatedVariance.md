# Get Aggregated Variance

This function calculates the aggregated variance based on the specified
aggregation function and identifier.

## Usage

``` r
getAggregatedVariance(
  dt,
  aggregationFun,
  valueColumn,
  identifier,
  direction = c("y", "x")
)
```

## Arguments

- dt:

  A data.table containing the data to aggregate. It must include the
  column specified in \`valueColumn\`.

- aggregationFun:

  A function to aggregate the data. This function should accept a
  numeric vector and return a list with aggregated values.

- valueColumn:

  A string indicating the column name containing the values to
  aggregate.

- identifier:

  A character vector specifying the columns to group by.

- direction:

  A character string specifying the direction of aggregation, either 'y'
  or 'x'.

## Value

A data.table containing the aggregated variance results, including the
aggregated values and error types.
