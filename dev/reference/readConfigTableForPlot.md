# Read Configuration Table for Plot

This function reads a configuration table from an Excel file, filters it
based on specified plot names, and merges scenario information. It also
validates the resulting configuration table using a provided validation
function.

## Usage

``` r
readConfigTableForPlot(
  projectConfiguration,
  sheetName,
  validateConfigTableFunction,
  inputs,
  plotNames
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration object

- sheetName:

  A character string specifying the name of the sheet to read from the
  Excel file. If NULL, the function returns NULL.

- validateConfigTableFunction:

  A function that validates the configuration table.

- inputs:

  A list of additional inputs to be passed to the validation function.

- plotNames:

  A character vector of plot names to filter the configuration table. If
  NULL, no filtering is applied.

## Value

A data.table containing the filtered and validated configuration table,
or NULL if sheetName is NULL.
