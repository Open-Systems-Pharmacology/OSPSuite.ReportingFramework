# Read data from a worksheet

This function wraps \`openxlsx::read.xlsx\` and returns the data as a
\`data.table\`.

## Usage

``` r
xlsxReadData(
  wb,
  sheetName = 1,
  skipDescriptionRow = FALSE,
  alwaysCharacter = c("Group", "Id$", "Ids$"),
  emptyAsNA = TRUE,
  convertHeaders = TRUE
)
```

## Arguments

- wb:

  A workbook object or a character string specifying the path to the
  xlsx file.

- sheetName:

  A character string specifying the name of the sheet to read.

- skipDescriptionRow:

  A logical value or an integer indicating whether the first row (or
  rows, if an integer) should be treated as description rows and skipped
  during reading. When set to TRUE, the first row is skipped; when set
  to a positive integer, that number of rows will be skipped. The
  'Comment' column is also excluded from the resulting data table.

- alwaysCharacter:

  A character vector with column names or regex patterns that should be
  returned as character (typically identifiers).

- emptyAsNA:

  A logical value. If TRUE, empty strings in character columns are
  converted to NA. If FALSE NA is returned as empty string. Numeric
  columns Return always NA

- convertHeaders:

  A logical value. If TRUE, column names are converted to start with a
  lowercase letter.

## Value

A \`data.table\` containing the sheet data.

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wb <- loadWorkbook("example.xlsx")
data <- xlsxReadData(wb, "DataSheet", skipDescriptionRow = 1)
print(data)
} # }
```
