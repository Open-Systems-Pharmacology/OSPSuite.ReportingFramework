# Filter Time Profile by Individual IDs

This function filters a given time profile data frame to include only
the rows corresponding to specified individual IDs. If the individual
list is not provided or the time profile has no column "individualId"
the original time profile is returned.

## Usage

``` r
filterIndividualID(timeprofile, individualList)
```

## Arguments

- timeprofile:

  A data frame containing time profile data with an \`individualId\`
  column.

- individualList:

  A character string of individual IDs to filter by. If the string
  contains '\*', all individuals will be included.

## Value

A data frame containing only the rows from \`timeprofile\` that match
the specified individual IDs. If no IDs are provided, the original
\`timeprofile\` is returned.
