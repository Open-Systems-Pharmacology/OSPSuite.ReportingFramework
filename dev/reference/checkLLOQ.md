# Sets values which do not match the LLOQ criteria to NA

Sets values which do not match the LLOQ criteria to NA

## Usage

``` r
checkLLOQ(
  aggregatedData,
  lloqCheckColumns2of3,
  lloqCheckColumns1of2,
  aggregationFlag
)
```

## Arguments

- aggregatedData:

  A \`data.table\` with aggregated data.

- lloqCheckColumns2of3:

  A character vector specifying columns to check for LLOQ (Lower Limit
  of Quantification) for 1/3 data points. Default is NULL, is used only
  for \`aggregationFlag\` "Custom".

- lloqCheckColumns1of2:

  A character vector specifying columns to check for LLOQ for 2/3 data
  points. Default is NULL, is used only for \`aggregationFlag\`
  "Custom".

- aggregationFlag:

  A character string indicating the aggregation method. Options include
  "GeometricStdDev", "ArithmeticStdDev", "Percentiles", or "Custom".

## Value

Updated aggregated data \`data.table\`.
