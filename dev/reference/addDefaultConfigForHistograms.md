# Add Default Configuration for Histograms

This function adds a default configuration sheet for histograms to the
plot configuration table. It can either create a new sheet or overwrite
an existing one based on the specified parameters.

## Usage

``` r
addDefaultConfigForHistograms(
  projectConfiguration,
  pkParameterDT = NULL,
  sheetName = "Histograms",
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
  configuration table. Default is "Histograms".

- overwrite:

  A boolean indicating whether existing configurations should be
  overwritten. Default is FALSE.

## Value

NULL This function updates the Excel workbook in place and does not
return a value. It is called for its side effects. Add Default
Configuration for Histograms

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
[`addDefaultConfigForDistributionsVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForDistributionsVsDemographics.md),
[`addDefaultConfigForPKBoxwhsikerPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKBoxwhsikerPlots.md),
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md)

Other functions to generate plots displaying distribution vs
demographics:
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md),
[`validateDistributionVsDemographicsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateDistributionVsDemographicsConfig.md)
