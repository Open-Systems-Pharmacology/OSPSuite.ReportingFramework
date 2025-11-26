# Validate Observed Data

Checks the integrity and validity of observed data in a \`data.table\`.
This function verifies the presence of required attributes, checks for
duplicates, and ensures that there are no missing or empty values in the
relevant columns. Additionally, it checks for ambiguities in the Y unit
and validates error type columns.

## Usage

``` r
validateObservedData(dataDT, dataClassType)
```

## Arguments

- dataDT:

  A \`data.table\` containing observed data with the following relevant
  columns: - \`individualId\`: Unique identifier for individuals. -
  \`group\`: Group identifier. - \`outputPathId\`: Identifier for output
  paths. - \`xValues\`: Values for the x-axis. - \`yUnit\`: Unit for the
  Y values. - \`yErrorType\`: Type of error for Y values (optional). -
  \`yErrorValues\`: Values representing errors (optional). - \`yMin\`,
  \`yMax\`: Minimum and maximum Y values (optional). - \`lloq\`: Lower
  limit of quantification (optional). - \`nBelowLLOQ\`: Count of values
  below the lower limit of quantification (optional).

- dataClassType:

  A string indicating the type of data class (e.g., "timeprofile" or
  "pkParameter").

## Value

NULL This function performs checks and stops execution if any validation
fails. It does not return a value.

## Details

The function performs several checks, including: - Ensuring all data
columns have the appropriate attributes. - Verifying that the data is
unique based on specified identifier columns. - Checking for NA or empty
values in all relevant columns. - Ensuring that the Y unit is consistent
across output paths. - Validating the presence of necessary columns
based on the Y error type.

## See also

Other observed data processing:
[`addBiometricsToConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addBiometricsToConfig.md),
[`aggregateObservedDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md),
[`convertDataCombinedToDataTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataCombinedToDataTable.md),
[`convertDataTableToDataCombined()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/convertDataTableToDataCombined.md),
[`readObservedDataByDictionary()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md),
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md),
[`updateOutputPathId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateOutputPathId.md)
