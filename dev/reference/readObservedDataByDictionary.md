# Read data by dictionary

This function reads and processes data based on the provided project
configuration.

## Usage

``` r
readObservedDataByDictionary(
  projectConfiguration,
  spreadData = TRUE,
  dataClassType = c("timeprofile", "pkParameter"),
  fileIds = NULL
)
```

## Arguments

- projectConfiguration:

  An object containing project configuration details, including the path
  to the data importer configuration file.

- spreadData:

  If TRUE, information derived from observed data, such as identifiers
  and biometrics, is spread to other tables.

- dataClassType:

  A character string indicating the type of data class to process.
  Options are "timeprofile" or "pkParameter".

- fileIds:

  A character vector with file identifiers to be selected, if NULL
  (default) all are selected.

## Value

A \`data.table\` containing the processed data based on the dictionary.
The structure includes relevant columns defined in the data dictionary.

## See also

Other observed data processing:
[`addBiometricsToConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addBiometricsToConfig.md),
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`convertDataTableToDataCombined()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataTableToDataCombined.md),
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md),
[`updateOutputPathId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateOutputPathId.md),
[`validateObservedData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateObservedData.md)
