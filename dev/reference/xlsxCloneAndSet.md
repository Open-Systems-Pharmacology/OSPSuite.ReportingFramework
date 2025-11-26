# Clone a sheet and set new content

This function wraps \`openxlsx::cloneWorksheet\` but sets new content in
the cloned sheet.

## Usage

``` r
xlsxCloneAndSet(wb, clonedSheet, sheetName, dt)
```

## Arguments

- wb:

  A workbook object created by \`openxlsx::loadWorkbook()\`.

- clonedSheet:

  A character string specifying the name of the sheet to clone.

- sheetName:

  A character string specifying the name of the new sheet.

- dt:

  A \`data.table\` with new content.

## Value

An invisible NULL value. The function performs an action (clone a sheet)

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wb <- loadWorkbook("example.xlsx")
data <- data.table(Name = c("Eve", "Frank"), Age = c(22, 35))
xlsxCloneAndSet(wb, "ExistingSheet", "ClonedSheet", data)
} # }
```
