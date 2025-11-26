# Clean character columns by trimming whitespace and replacing curly quotes

This helper function trims whitespace from character columns and
replaces curly quotes with straight quotes.

## Usage

``` r
cleanCharacterColumns(dt, emptyAsNA)
```

## Arguments

- dt:

  A \`data.table\` to process.

- emptyAsNA:

  A logical value. If TRUE, empty strings in character columns are
  converted to NA. If FALSE NA is returned as empty string.

## Value

A \`data.table\` with cleaned character columns.
