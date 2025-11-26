# Update Data Group IDs in Project Configuration

This function updates the Data Groups sheet in an Excel workbook based
on a provided data table. It reads existing data group IDs, merges them
with new data, and ensures that the identifiers remain unique.

## Usage

``` r
updateDataGroupId(projectConfiguration, dataDT)
```

## Arguments

- projectConfiguration:

  An object of class \`ProjectConfiguration\` containing configuration
  details

- dataDT:

  A \`data.table\` containing the new data to be added to the Data
  Groups sheet. It must have columns that can be matched with existing
  identifiers, including "group", "studyId", and "studyArm".

## Value

NULL This function updates the Excel workbook in place and does not
return a value. It is called for its side effects.

## Details

The function loads the existing Data Groups from the specified Excel
workbook, selects relevant columns from the input data, and appends new
entries while maintaining the uniqueness of the identifiers. The updated
data is then written back to the workbook.

## See also

Other observed data processing:
[`addBiometricsToConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addBiometricsToConfig.md),
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`convertDataTableToDataCombined()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataTableToDataCombined.md),
[`readObservedDataByDictionary()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md),
[`updateOutputPathId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateOutputPathId.md),
[`validateObservedData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateObservedData.md)
