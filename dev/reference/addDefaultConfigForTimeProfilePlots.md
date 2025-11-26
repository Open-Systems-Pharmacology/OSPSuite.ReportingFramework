# Add Default Configuration for Time Profile Plots

This function adds a default configuration sheet for time profile plots
to the plot configuration table. It can either create a new sheet or
overwrite an existing one based on the specified parameters.

## Usage

``` r
addDefaultConfigForTimeProfilePlots(
  projectConfiguration,
  dataObserved = NULL,
  sheetName = "TimeProfiles",
  overwrite = FALSE
)
```

## Arguments

- projectConfiguration:

  A \`ProjectConfiguration\` class object containing configuration
  details, including: - \`plotsFile\`: A string representing the file
  path to the Excel workbook containing the plot configurations.

- dataObserved:

  Optional. A data object containing observed data, if available.

- sheetName:

  A character string specifying the name of the sheet in the plot
  configuration table. Default is \`TimeProfiles\`.

- overwrite:

  A boolean indicating whether existing configurations should be
  overwritten. Default is FALSE.

## Value

NULL This function updates the Excel workbook in place and does not
return a value. It is called for its side effects.

## Details

The function retrieves scenario definitions, output path IDs, and data
groups from the project configuration. It checks if the specified sheet
already exists and whether to overwrite it. If not, it creates a new
header and fills in the default configuration values for the time
profile plots.

Additionally, the function performs a validity check to ensure that it
is not executed during a context where helper functions are prohibited
(\`validRun\`). If such a context is detected, an error is raised to
prevent execution.

## See also

Other plot functions:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md),
[`plotHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotHistograms.md),
[`plotPKBoxwhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKBoxwhisker.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotSensitivity()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotSensitivity.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)

Other plot configuration helper function:
[`addDefaultConfigForDistributionsVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForDistributionsVsDemographics.md),
[`addDefaultConfigForHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForHistograms.md),
[`addDefaultConfigForPKBoxwhsikerPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKBoxwhsikerPlots.md),
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md)
