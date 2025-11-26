# Get Aggregation Function

This function retrieves the appropriate aggregation function based on
the specified aggregation flag.

## Usage

``` r
getAggregationFunction(
  aggregationFlag,
  percentiles,
  customFunction,
  legendsize = 2
)
```

## Arguments

- aggregationFlag:

  A character string indicating the aggregation method. Must be one of
  the options from \`ospsuite::DataErrorType\`, "Percentiles", or
  "Custom".

- percentiles:

  A numeric vector of percentiles to calculate if \`aggregationFlag\` is
  "Percentiles". Must have a length of 3, be sorted, and within the
  range \[0, 1\].

- customFunction:

  A custom function for aggregation if \`aggregationFlag\` is "Custom".
  A custom function should take a numeric vector \`y\` as input and
  return a list containing: - \`yValues\`: The aggregated value (e.g.,
  mean). - \`yMin\`: The lower value of the aggregated data, (e.g.
  mean - sd). - \`yMax\`: The upper value of the aggregated data, (e.g.
  mean + sd). - \`yErrorType\`: A string indicating the type of error
  associated with the aggregation, it is used in plot legends and
  captions. It must be a concatenation of the descriptor of \`yValues\`
  and the descriptor of \`yMin\` - \`yMax\` range separated by "\|"
  (e.g., "mean \| standard deviation" or "median \| 5th - 95th
  percentile"). If legendsize 3 is needed should contain 3 elements,
  e.g. "median \| 5th percentile \| 95th percentile"

- legendsize:

  An integer indicating the size of the legend vector. Supported values
  are 2 or 3, which correspond to different formats for displaying
  percentile results.

## Value

A function that performs the specified aggregation. The returned
function accepts a numeric vector and returns a list containing the
aggregated values and error types.
