# Process the data read from the sheet

This helper function processes the data by skipping description rows,
converting specified columns to character, converting empty strings to
NA, and adjusting column headers.

## Usage

``` r
processSheetData(
  dt,
  skipDescriptionRow,
  alwaysCharacter,
  emptyAsNA,
  convertHeaders
)
```

## Arguments

- dt:

  A \`data.table\` containing the raw data.

- skipDescriptionRow:

  A logical value or an integer indicating whether to skip description
  rows.

- alwaysCharacter:

  A character vector with column names or regex patterns that should be
  returned as character.

- emptyAsNA:

  A logical value. If TRUE, empty strings in character columns are
  converted to NA. If FALSE NA is returned as empty string. Numeric
  columns Return always NA

- convertHeaders:

  A logical value. If TRUE, column names are converted to start with a
  lowercase letter.

## Value

A processed \`data.table\`.
