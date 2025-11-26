# Add Default Configuration for for distribution vs demographics plots

This function adds a default configuration sheet for distribution vs
demographics plots to the plot configuration table. It can either create
a new sheet or overwrite an existing one based on the specified
parameters.

## Usage

``` r
addDefaultConfigForDistributionsVsDemographics(
  projectConfiguration,
  pkParameterDT = NULL,
  sheetName = "DistributionVsRange",
  overwrite = FALSE
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration class object containing configuration details,
  including: - \`plotsFile\`: A string representing the file path to the
  Excel workbook containing the plot configurations.

- pkParameterDT:

  Optional. A data object containing pkParameter.

- sheetName:

  A character string specifying the name of the sheet in the plot
  configuration table. Default is \`DistributionVsRange\`.

- overwrite:

  A boolean indicating whether existing configurations should be
  overwritten. Default is FALSE.

## Value

NULL This function updates the Excel workbook in place and does not
return a value.

## Details

The function retrieves scenario definitions, output path IDs, and data
groups from the project configuration. It checks if the specified sheet
already exists and whether to overwrite it. If not, it creates a new
header and fills in the default configuration values for the histograms.

Additionally, the function performs a validity check to ensure that it
is not executed during a context where helper functions are prohibited
(validRun). If such a context is detected, an error is raised to prevent
execution.

## See also

Other plot configuration helper function:
[`addDefaultConfigForHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForHistograms.md),
[`addDefaultConfigForPKBoxwhsikerPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKBoxwhsikerPlots.md),
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md)

Other functions to generate histograms:
[`plotHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotHistograms.md),
[`validateHistogramsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateHistogramsConfig.md)
