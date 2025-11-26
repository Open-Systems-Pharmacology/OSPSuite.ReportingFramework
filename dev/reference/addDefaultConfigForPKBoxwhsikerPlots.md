# Add Default Configuration for Box-and-Whisker Plots

This function adds a default configuration sheet for box-and-whisker
plots to the plot configuration table. It can either create a new sheet
or overwrite an existing one based on the specified parameters.

## Usage

``` r
addDefaultConfigForPKBoxwhsikerPlots(
  projectConfiguration,
  pkParameterDT,
  sheetName = "PKParameter_Boxplot",
  overwrite = FALSE
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration class object containing configuration details,
  including: - \`plotsFile\`: A string representing the file path to the
  Excel workbook containing the plot configurations.

- pkParameterDT:

  A data.table containing PK parameter data.

- sheetName:

  A character string specifying the name of the sheet in the plot
  configuration table. Default is "PKParameter_Boxplot".

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

Other plot configuration helper function:
[`addDefaultConfigForDistributionsVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForDistributionsVsDemographics.md),
[`addDefaultConfigForHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForHistograms.md),
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md)

Other functions to generate box whisker plots:
[`plotPKBoxwhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKBoxwhisker.md),
[`validatePKBoxwhiskerConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKBoxwhiskerConfig.md)
