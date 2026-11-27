# Write data to a worksheet or add the worksheet if it does not exist

This function centralizes the workbook mutation logic for callers that
want to update an existing sheet when present or create a new one
otherwise.

## Usage

``` r
xlsxWriteOrAddData(wb, sheetName, dt)
```

## Arguments

- wb:

  A workbook object created by \`openxlsx::loadWorkbook()\`.

- sheetName:

  A character string specifying the target sheet name.

- dt:

  A \`data.frame\` or \`data.table\` to write.

## Value

An invisible NULL value. The function performs an action on the
workbook.

## See also

Other function to read from and write to xlsx:
[`copyConfigSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/copyConfigSheet.md),
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wb <- loadWorkbook("example.xlsx")
data <- data.frame(Name = c("Charlie", "Dana"), Age = c(28, 32))
xlsxWriteOrAddData(wb, "ExistingOrNewSheet", data)
} # }
```
