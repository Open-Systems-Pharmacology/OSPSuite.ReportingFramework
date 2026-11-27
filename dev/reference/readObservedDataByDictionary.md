# Read data by dictionary

This function reads and processes data based on the provided project
configuration. Depending on \`spreadData\`, it can also propagate
identifiers and metadata into reporting-configuration workbooks.

## Usage

``` r
readObservedDataByDictionary(
  projectConfiguration,
  spreadData = TRUE,
  dataClassType = c("timeprofile", "pkParameter", "numericValues"),
  fileIds = NULL,
  debugMode = FALSE
)
```

## Arguments

- projectConfiguration:

  An object containing project configuration details, including the path
  to the data importer configuration file.

- spreadData:

  If \`TRUE\`, information derived from observed data is spread to
  configuration workbooks: \`Reports.xlsx\` (\`DataGroups\`,
  \`Outputs\`, and synchronized \`Scenarios\`) and \`Individuals.xlsx\`
  (\`IndividualBiometrics\`, \`VirtualTwinPopulation\`). If \`FALSE\`,
  import remains read-only and no configuration workbook is modified.

- dataClassType:

  A character string indicating the type of data class to process.
  Options are "timeprofile", "pkParameter", or "numericValues".

- fileIds:

  A character vector with file identifiers to be selected, if NULL
  (default) all are selected.

- debugMode:

  Logical. If \`TRUE\`, import errors are downgraded to warnings on a
  per-file basis where possible, and processing continues with remaining
  files. This mode is intended for exploratory runs and is blocked
  during valid runs. Default is \`FALSE\`.

## Value

A \`data.table\` containing the processed data based on the dictionary.
The structure includes relevant columns defined in the data dictionary.

## Details

The import pipeline is split into discrete stages: selecting configured
files by class, importing each file by its dictionary mapping, applying
dictionary-based conversions, validating the combined output, and
optionally propagating discovered identifiers into configuration
workbooks.

## See also

Other observed data processing:
[`addBiometricsToConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addBiometricsToConfig.md),
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`convertDataTableToDataCombined()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataTableToDataCombined.md),
[`getNumericValue()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getNumericValue.md),
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md),
[`updateOutputPathId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateOutputPathId.md),
[`validateObservedData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateObservedData.md)
