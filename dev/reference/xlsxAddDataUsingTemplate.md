# Add new data to a config file using a template sheet

If the template does not exist in the configuration file in the project
directory, it is taken from the configuration file of the package
installation. In this case, formats are not preserved.

## Usage

``` r
xlsxAddDataUsingTemplate(
  wb,
  templateSheet,
  sheetName,
  dtNewData,
  templateXlsx = "Plots.xlsx"
)
```

## Arguments

- wb:

  Current configuration file.

- templateSheet:

  Name of the template sheet.

- sheetName:

  Name of the new sheet.

- dtNewData:

  A \`data.table\` with new data.

- templateXlsx:

  A character string specifying the template xlsx file name (default is
  "Plots.xlsx").

## Value

The current configuration file with the added sheet.

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wb <- loadWorkbook("config.xlsx")
newData <- data.table(Name = c("Gina", "Hank"), Age = c(29, 33))
xlsxAddDataUsingTemplate(wb, "TemplateSheet", "NewDataSheet", newData)
} # }
```
