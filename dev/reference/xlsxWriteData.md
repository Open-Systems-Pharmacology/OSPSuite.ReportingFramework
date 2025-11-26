# Write data to a worksheet, clearing existing content

This function wraps \`openxlsx::writeData\`, but deletes all current
content before writing new data.

## Usage

``` r
xlsxWriteData(wb, sheetName, dt)
```

## Arguments

- wb:

  A workbook object created by \`openxlsx::loadWorkbook()\`.

- sheetName:

  A character string specifying the name of the sheet where data will be
  written.

- dt:

  A \`data.table\` to write.

## Value

An invisible NULL value. The function performs an action (adding data to
a sheet)

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wb <- loadWorkbook("example.xlsx")
data <- data.table(Name = c("Charlie", "Dana"), Age = c(28, 32))
xlsxWriteData(wb, "ExistingSheet", data)
} # }
```
