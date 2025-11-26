# Converts data.table with observed data to \`ospsuite::DataCombined\` object

The \`data.table\` must be formatted like a table produced by
\`readObservedDataByDictionary\`.

## Usage

``` r
convertDataTableToDataCombined(dataDT)
```

## Arguments

- dataDT:

  A \`data.table\` to convert.

## Value

An object of class \`DataCombined\`.

## See also

Other observed data processing:
[`addBiometricsToConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addBiometricsToConfig.md),
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`readObservedDataByDictionary()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md),
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md),
[`updateOutputPathId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateOutputPathId.md),
[`validateObservedData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateObservedData.md)
