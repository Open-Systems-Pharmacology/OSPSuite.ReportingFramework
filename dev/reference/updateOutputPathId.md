# Update Output Path IDs in Project Configuration

This function updates the Outputs sheet in an Excel workbook based on a
provided data table, ensuring that output path IDs remain unique.

## Usage

``` r
updateOutputPathId(projectConfiguration, dataDT)
```

## Arguments

- projectConfiguration:

  An object of class \`ProjectConfiguration\` containing configuration
  details.

- dataDT:

  A \`data.table\` containing new output path IDs to be added to the
  Outputs sheet. It must include a column named "outputPathId".

## Value

NULL This function updates the Excel workbook in place and does not
return a value. It is called for its side effects.

## Details

The function loads the existing Output Paths from the specified Excel
workbook, extracts unique output path IDs from the input data, and
appends them while maintaining uniqueness. The updated data is then
written back to the workbook.

## See also

Other observed data processing:
[`addBiometricsToConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addBiometricsToConfig.md),
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`convertDataTableToDataCombined()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataTableToDataCombined.md),
[`readObservedDataByDictionary()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md),
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md),
[`validateObservedData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateObservedData.md)
