# Validates time range columns

Time range columns must be character and must contain NA
'total','firstApplication','lastApplication' or a string which evaluates
in R to a numeric vector length 2 (e.g. 'c(2,3)' or 'c(2,NA)'

## Usage

``` r
validateTimeRangeColumns(configTablePlots)
```

## Arguments

- configTablePlots:

  \`data.table\` configuration table without header lines
