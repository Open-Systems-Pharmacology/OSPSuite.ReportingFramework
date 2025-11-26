# Adjust the dimensions of a data.table to match existing data

This function adjusts the dimensions of a new data.table to match the
existing data. If the existing data has more rows, it appends NA rows to
the new data.

## Usage

``` r
adjustDataTableDimensions(dt, existingData)
```

## Arguments

- dt:

  A data.table containing the new data to be written.

- existingData:

  A data.table containing the existing data in the specified sheet.

## Value

A data.table with adjusted dimensions.
