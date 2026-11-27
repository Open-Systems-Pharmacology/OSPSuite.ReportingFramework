# Copy a configuration sheet between workbooks

Copies one worksheet from a source workbook into a target workbook. If
source and target are the same workbook and the target sheet does not
exist, the function clones the sheet to preserve formatting.

## Usage

``` r
copyConfigSheet(
  sourceFile,
  sourceSheetName,
  targetFile = sourceFile,
  targetSheetName = sourceSheetName,
  overwrite = FALSE
)
```

## Arguments

- sourceFile:

  Path to the source xlsx workbook.

- sourceSheetName:

  A character string specifying the source sheet name.

- targetFile:

  Path to the target xlsx workbook. Defaults to \`sourceFile\`.

- targetSheetName:

  A character string specifying the target sheet name. Defaults to
  \`sourceSheetName\`.

- overwrite:

  Logical value indicating whether an existing target sheet should be
  overwritten.

## Value

An invisible NULL value. The function performs an action (copy a sheet
between workbooks).

## Details

For copies within the same workbook, the function clones missing target
sheets to retain formatting. For cross-workbook copies, it transfers
table content by reading from source and writing into the target sheet.

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md),
[`xlsxWriteOrAddData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteOrAddData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
copyConfigSheet(
  sourceFile = "Scenarios.xlsx",
  sourceSheetName = "Scenarios",
  targetFile = "Reports.xlsx",
  targetSheetName = "Scenarios"
)
} # }
```
