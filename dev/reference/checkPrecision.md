# Check Precision of xValues

This function calculates the precision of xValues in a data.table based
on the provided xMin and xMax columns. If the precision is below a
specified threshold, it sets xMin and xMax to NA and calculates an
estimated N value. It also logs a message if the required precision is
not reached.

## Usage

``` r
checkPrecision(dt)
```

## Arguments

- dt:

  A data.table containing the columns xValues, xMin, and xMax.

## Value

A data.table with updated xMin and xMax values, and an additional column
estimatedN if the required precision is not reached.

## Details

The precision is calculated as xValues / (xMax - xMin). The precision
threshold is retrieved from the options with a default value of 0.01. If
any xMin values remain, a message is logged indicating that the required
precision was not reached.
