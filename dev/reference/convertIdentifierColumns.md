# Update identifier columns in a data.table

This function updates the specified identifier columns in a
\`data.table\` by replacing commas with underscores. It also checks for
the presence of commas in the columns and generates a warning message if
found.

## Usage

``` r
convertIdentifierColumns(dt, identifierCols)
```

## Arguments

- dt:

  The input \`data.table\`.

- identifierCols:

  A character vector specifying the columns to be updated.

## Value

The updated \`data.table\`.
