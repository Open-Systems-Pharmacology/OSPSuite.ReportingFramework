# Select all columns where the attribute \`columnType\` matches the requirement

Select all columns where the attribute \`columnType\` matches the
requirement

## Usage

``` r
getColumnsForColumnType(dt, columnTypes)
```

## Arguments

- dt:

  A \`data.table\` with attributes (e.g., imported by
  \`readObservedDataByDictionary\`).

- columnTypes:

  A vector with required types.

## Value

A vector with column names.
