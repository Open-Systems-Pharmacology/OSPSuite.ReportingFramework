# Convert convertible columns to numeric

This helper function converts columns in a data.table to numeric if
possible.

## Usage

``` r
convertColumnsToNumeric(dt, alwaysCharacter)
```

## Arguments

- dt:

  A \`data.table\` to process.

- alwaysCharacter:

  A character vector of column names to remain as character.

## Value

A \`data.table\` with numeric conversions applied where appropriate.
