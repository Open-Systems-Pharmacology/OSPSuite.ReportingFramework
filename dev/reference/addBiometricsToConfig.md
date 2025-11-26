# Add biometrics information to config

Add biometrics information to config

## Usage

``` r
addBiometricsToConfig(projectConfiguration, dataDT, overwrite = FALSE)
```

## Arguments

- projectConfiguration:

  Object of class \`ProjectConfiguration\` containing information on
  paths and file names

- dataDT:

  A \`data.table\` with observed data.

- overwrite:

  If TRUE, existing rows will be overwritten.

## See also

Other observed data processing:
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`convertDataTableToDataCombined()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataTableToDataCombined.md),
[`readObservedDataByDictionary()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md),
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md),
[`updateOutputPathId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateOutputPathId.md),
[`validateObservedData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateObservedData.md)
