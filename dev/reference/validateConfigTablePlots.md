# validate types of plot configuration tables

validate types of plot configuration tables

## Usage

``` r
validateConfigTablePlots(
  configTablePlots,
  charactersWithoutMissing = NULL,
  charactersWithMissing = NULL,
  numericColumns = NULL,
  logicalColumns = NULL,
  numericRangeColumns = NULL,
  subsetList = list()
)
```

## Arguments

- configTablePlots:

  \`data.table\` configuration table without header lines

- charactersWithoutMissing:

  vector with character columns, where no missing value is allowed

- charactersWithMissing:

  vector with character column, where values may missing

- numericColumns:

  vector with numeric columns

- logicalColumns:

  vector with booleans

- numericRangeColumns:

  vector with columns where entries must be evaluate to a numeric of
  length 2

- subsetList:

  list where each entry is a list: list(cols = 'vector with columns',
  allowedValues = vector with allowed values)
