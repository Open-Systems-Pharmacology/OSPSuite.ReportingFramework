# Read data dictionary

This function reads the data dictionary based on the provided file and
sheet.

## Usage

``` r
readDataDictionary(dictionaryFile, sheet, data, dataClass)
```

## Arguments

- dictionaryFile:

  The file containing the data dictionary.

- sheet:

  The sheet within the data dictionary file.

- data:

  The data to be used with the dictionary.

- dataClass:

  Class of data, either "tp Individual" or "tp Aggregated".

## Value

A \`data.table\` containing the data dictionary.
