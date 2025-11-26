# Align column names of a data.table with existing data

This function aligns the column names of a new data.table with those of
the existing data. It ensures that the new data has the same column
names as the existing data, handling potential ambiguities.

## Usage

``` r
alignColumnNames(dt, existingData)
```

## Arguments

- dt:

  A data.table containing the new data to be written.

- existingData:

  A data.table containing the existing data in the specified sheet.

## Value

A data.table with aligned column names.
