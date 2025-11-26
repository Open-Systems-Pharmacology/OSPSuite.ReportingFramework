# Update Data Table Caption

This function updates the provided data table caption by modifying the
individual IDs based on a given configuration table. If the
configuration table contains any individual IDs marked with '(\*)',
those IDs will be removed from the data table caption.

## Usage

``` r
updateDtCaption(dtCaption, configTable)
```

## Arguments

- dtCaption:

  A data.table containing the captions to be updated.

- configTable:

  A data.table containing configuration information, including
  individual IDs.

## Value

A data.table with updated captions, where individual IDs marked with
'(\*)' are removed.
