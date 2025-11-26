# Determine if Export Should be Suppressed

This helper function checks whether the export of the Rmd file should be
suppressed based on user-defined conditions.

## Usage

``` r
shouldSuppressExport(suppressExport, plotNames, inputs)
```

## Arguments

- suppressExport:

  A logical value indicating the current state of export suppression.

- plotNames:

  A character vector of plot names to filter which plots should be
  generated. Default is NULL.

- inputs:

  A list of additional inputs that may contain parameters influencing
  the export behavior.

## Value

A logical value indicating whether the export should be suppressed
(TRUE) or not (FALSE).
