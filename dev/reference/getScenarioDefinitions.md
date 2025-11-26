# Load the Scenario Definitions

This function loads the scenario definitions from the specified
workbooks.

## Usage

``` r
getScenarioDefinitions(wbScenarios, wbPlots = NULL)
```

## Arguments

- wbScenarios:

  The path to the workbook containing additional scenario definitions.

- wbPlots:

  The path to the workbook containing the scenario definitions.

## Value

A \`data.table\` with scenario definitions.

## See also

Other get identifier:
[`getDataGroups()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getDataGroups.md),
[`getModelParameterDefinitions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getModelParameterDefinitions.md),
[`getOutputPathIds()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getOutputPathIds.md),
[`getTimeRangeTags()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getTimeRangeTags.md),
[`loadConfigTableEnvironment()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadConfigTableEnvironment.md)
