# Calculate Aggregation and Confidence Interval by Group

This function calculates a specified aggregation function (e.g.,
geometric mean) and the associated confidence interval using
bootstrapping, grouped by specified identifiers. The function allows for
flexible definitions of the aggregation function and can handle
different value columns and output formats based on the specified
direction. A unique seed for bootstrapping is generated for each group
of identifiers to ensure reproducibility of the bootstrap samples.

## Usage

``` r
calculateAggregationWithCIBYGroup(
  dt,
  aggregationFun,
  confLevel = 0.9,
  identifier,
  nBootstrap = 100,
  valueColumn = "value",
  direction = "y"
)
```

## Arguments

- dt:

  A data.table containing the data. It must include the columns
  specified in \`identifier\` and the \`valueColumn\`.

- aggregationFun:

  A function to calculate the aggregation (e.g., geometric mean). This
  function should accept a numeric vector and return a single numeric
  value.

- confLevel:

  A numeric value representing the confidence level for the confidence
  interval. The default value is 0.9, corresponding to a 90percent
  confidence interval. Must be between 0 and 1.

- identifier:

  A character vector of column names in the data.table to group by. The
  function will calculate the aggregation and confidence interval for
  each unique combination of these identifiers.

- nBootstrap:

  An integer specifying the number of bootstrap samples to use. The
  default is 100.

- valueColumn:

  The name of the column containing the values to aggregate. The default
  is 'value'.

- direction:

  A character string indicating the direction of the results. It can be
  either 'y' (default) or 'x'. This affects how the results are named in
  the output data.table.

## Value

A data.table containing the following columns: - \`yValues\`: The
estimated value from the aggregation function. - \`yMin\`: The lower
bound of the confidence interval. - \`yMax\`: The upper bound of the
confidence interval. - \`seed\`: The seed used for bootstrapping,
derived from the identifiers. - \`yErrorType\`: A descriptive string
indicating the aggregation function and confidence interval bounds.
