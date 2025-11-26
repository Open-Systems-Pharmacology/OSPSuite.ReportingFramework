# This function checks if the \`colorLegend\` column in a data.table contains valid entries. Each entry must be a character string concatenated from two characters separated by a pipe (\`\|\`). If any entry does not meet this requirement, the function will print the invalid entries and stop execution.

This function checks if the \`colorLegend\` column in a data.table
contains valid entries. Each entry must be a character string
concatenated from two characters separated by a pipe (\`\|\`). If any
entry does not meet this requirement, the function will print the
invalid entries and stop execution.

## Usage

``` r
validateColorLegend(dt)
```

## Arguments

- dt:

  A data.table containing a column named \`colorLegend\` and optionally
  a \`plotName\` column.

## Value

NULL The function stops execution if invalid entries are found.
