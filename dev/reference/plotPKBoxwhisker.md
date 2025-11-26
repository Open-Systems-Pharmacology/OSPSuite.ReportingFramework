# Plot PK Box-and-Whisker

Generates box-and-whisker plots for pharmacokinetic (PK) parameters
based on a provided project configuration and plot configuration. This
function creates visual representations of the distribution of PK
parameters across different scenarios, allowing for comparison of
absolute values and ratios.

## Usage

``` r
plotPKBoxwhisker(
  projectConfiguration,
  onePlotConfig,
  pkParameterDT,
  percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles),
  xAxisTextAngle = 0,
  colorVector = c(scenario = NA, referenceScenario = NA),
  facetAspectRatio = 0.5,
  ...
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration object that contains settings and paths
  relevant to the project.

- onePlotConfig:

  A data.table containing configuration settings for a single plot,
  including details about which plots to generate and their formatting.

- pkParameterDT:

  A data.table containing PK parameter data.

- percentiles:

  A numeric vector specifying the percentiles to calculate for the
  box-and-whisker plots. Default is retrieved from project options.

- xAxisTextAngle:

  An integer specifying the angle (in degrees) for rotating the x-axis
  text labels. Default is 0 (no rotation).

- colorVector:

  A named vector specifying colors for the plots. Names should
  correspond to the characters defined in the configtable column
  colorLegend. If your color legend is for example 'DDI\|control', your
  color vector could be colorVector = c(DDI = 'red',control = 'blue'),
  if no color is defined default values are used.

- facetAspectRatio:

  A numeric value specifying the aspect ratio for faceting the plots.
  Default is 0.5, which may need adjustment based on the number of
  facets and plot dimensions.

- ...:

  Additional arguments passed to plotting functions for further
  customization.

## Value

A list of ggplot objects representing the generated plots. Each plot can
be rendered using ggplot2 or similar plotting systems. The list may
include both absolute and ratio plots depending on the configuration.

## Details

The function is intended to be used in combination with the \`runPlot\`
function. See the vignette \`Plot and Report Generation\` for more
details.

There exists a helper function \`addDefaultConfigForPKBoxwhsikerPlots\`
which is a helpful utility designed to streamline the process of setting
up your Excel configuration sheet for box whisker plots. This function
automatically populates a new sheet with default values that you can
customize according to your needs.

## See also

Other plot functions:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md),
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md),
[`plotHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotHistograms.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotSensitivity()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotSensitivity.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)

Other functions to generate box whisker plots:
[`addDefaultConfigForPKBoxwhsikerPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKBoxwhsikerPlots.md),
[`validatePKBoxwhiskerConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKBoxwhiskerConfig.md)

## Examples

``` r
# Example usage of plotPKBoxwhisker
if (FALSE) { # \dontrun{
plotList <- plotPKBoxwhisker(
  projectConfiguration = myProjectConfig,
  onePlotConfig = myPlotConfig,
  pkParameterDT = myPKData,
  percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  xAxisTextAngle = 45,
  colorVector = c(DDI = "red", control = "blue"),
  facetAspectRatio = 1,
)
} # }
```
