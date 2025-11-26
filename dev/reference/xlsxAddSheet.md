# Add a new sheet to a workbook with data

This function wraps \`openxlsx::addWorksheet\` and adds data to the
newly created sheet. If the sheet already exists, it issues a warning
and clears the existing content.

## Usage

``` r
xlsxAddSheet(wb, sheetName, dt)
```

## Arguments

- wb:

  A workbook object created by \`openxlsx::loadWorkbook()\`.

- sheetName:

  A character string specifying the name of the sheet to add.

- dt:

  A \`data.table\` containing the data to be written to the new sheet.

## Value

An invisible NULL value. The function performs an action (adding a
sheet)

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(openxlsx)
wb <- loadWorkbook("example.xlsx")
data <- data.table(Name = c("Alice", "Bob"), Age = c(30, 25))
xlsxAddSheet(wb, "NewSheet", data)
} # }
```
