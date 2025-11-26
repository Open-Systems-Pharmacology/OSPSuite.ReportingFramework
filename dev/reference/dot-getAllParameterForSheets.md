# Get All Parameters for Sheets

This function retrieves parameters from specified sheets in an Excel
file.

## Usage

``` r
.getAllParameterForSheets(projectConfiguration, sheets, paramsXLSpath, sim)
```

## Arguments

- projectConfiguration:

  A list containing project configuration details.

- sheets:

  A character vector of sheet names to read parameters from.

- paramsXLSpath:

  A string representing the path to the parameters Excel file.

- sim:

  A simulation object.

## Value

A list of parameters for the specified sheets.
