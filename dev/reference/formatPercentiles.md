# This function takes a numeric vector of percentiles and maps specific values to their corresponding labels. It also formats other numeric values based on whether they are integers or not.

This function takes a numeric vector of percentiles and maps specific
values to their corresponding labels. It also formats other numeric
values based on whether they are integers or not.

## Usage

``` r
formatPercentiles(percentiles, suffix = "", allAsPercentiles = FALSE)
```

## Arguments

- percentiles:

  A numeric vector of percentiles (0, 50, 100, and other values).

- suffix:

  A character string to append to formatted percentile values.

- allAsPercentiles:

  boolean, if FALSE for percentiles = 0,50,100 min, median and max is
  returned otherwise 0th 50th 100th

## Value

A vector containing the mapped labels for specific percentiles and
formatted strings for others.

## Examples

``` r
formatPercentiles(c(0, 0.5, 1))
#> [1] "min"    "median" "max"   
formatPercentiles(c(0.05, 0.25, 0.75, 0.95), suffix = " CI")
#> [1] "5th CI"  "25th CI" "75th CI" "95th CI"
```
