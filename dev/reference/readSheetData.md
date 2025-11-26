# Read data from the specified sheet using openxlsx

This helper function reads data from a specified sheet in a workbook.

## Usage

``` r
readSheetData(wb, sheetName)
```

## Arguments

- wb:

  A workbook object created by \`openxlsx::loadWorkbook()\`.

- sheetName:

  A character string specifying the name of the sheet to read.

## Value

A \`data.table\` containing the raw data from the sheet.
