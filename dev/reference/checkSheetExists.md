# Check if the specified sheet exists in the workbook

This function checks if the given sheet name exists in the workbook.

## Usage

``` r
checkSheetExists(wb, sheetName)
```

## Arguments

- wb:

  A workbook object created by \`openxlsx::loadWorkbook()\`.

- sheetName:

  A character string specifying the name of the sheet to check.

## Value

NULL. If the sheet does not exist, an error is thrown.
