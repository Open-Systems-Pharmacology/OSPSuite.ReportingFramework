# Creates text for percentiles

This function generates a descriptive text based on the provided
percentiles.

## Usage

``` r
getErrorTypeForPercentiles(percentiles, legendsize)
```

## Arguments

- percentiles:

  A numeric vector containing percentiles. Must be of length 3.

- legendsize:

  An integer indicating the size of the legend vector. Supported values
  are 2 or 3, which affect the format of the output string.

## Value

A character string with the error type for the given percentiles,
formatted according to the specified legendsize.
