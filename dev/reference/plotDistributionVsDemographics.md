# Plot Distribution vs Demographics

Generates plots comparing distributions against demographic data based
on the provided configuration and data.

## Usage

``` r
plotDistributionVsDemographics(
  projectConfiguration,
  onePlotConfig,
  pkParameterDT = NULL,
  scenarioList = NULL,
  asStepPlot = TRUE,
  aggregationFlag = c("Percentiles", "GeometricStdDev", "ArithmeticStdDev", "Custom"),
  customFunction = NULL,
  percentiles = ospsuite.plots::getOspsuite.plots.option(optionKey =
    OptionKeys$Percentiles)[c(1, 3, 5)],
  facetAspectRatio = 0.5,
  colorVector = c(scenario = NA, referenceScenario = NA),
  ...
)
```

## Arguments

- projectConfiguration:

  A configuration object for the project, containing necessary settings.

- onePlotConfig:

  A configuration table for the specific plot, detailing plot
  parameters.

- pkParameterDT:

  A data.table containing pharmacokinetic (PK) parameter data
  (optional).

- scenarioList:

  A list of scenarios to consider for the plot (optional).

- asStepPlot:

  Logical indicating if the plot should be rendered as a step plot
  (default is TRUE).

- aggregationFlag:

  A character vector specifying the aggregation function for simulated
  data. Options include "GeometricStdDev" (default), "ArithmeticStdDev",
  "Percentiles", and "Custom".

- customFunction:

  Optional. A custom aggregation function that accepts a numeric vector
  and returns a list with: - \`yValues\`: The aggregated value (e.g.,
  mean). - \`yMin\`: The lower bound of the aggregated data (e.g.,
  mean - sd). - \`yMax\`: The upper bound of the aggregated data (e.g.,
  mean + sd). - \`yErrorType\`: A string indicating the error type for
  the aggregation, used for plot legends.

- percentiles:

  A numeric vector of percentiles to consider if the aggregation method
  is set to "Percentiles" (default is c(5, 50, 95)).

- facetAspectRatio:

  A numeric value for the aspect ratio of the facets (default is 0.5).

- colorVector:

  A named vector for colors corresponding to scenarios (default is
  c(scenario = NA, referenceScenario = NA)).

- ...:

  Additional arguments passed to the plotting functions
  \`ospsuite.plots::plotHistogram\` or \`plotRangeDistribution\`.

## Value

A list of plot objects generated based on the input configuration.

## Details

This function is intended to be used in combination with the \`runPlot\`
function. Refer to the vignette \`Plot and Report Generation\` for more
details. It can be configured to plot PK parameters or population
parameters, with the X-axis always displaying the population parameter.

A helper function, \`addDefaultConfigForDistributionsVsDemographics\`,
is available to streamline the setup of your Excel configuration sheet
for histograms. This function automatically populates a new sheet with
default values that can be customized as needed.

## See also

Other plot functions:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md),
[`plotHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotHistograms.md),
[`plotPKBoxwhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKBoxwhisker.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotSensitivity()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotSensitivity.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)

Other functions to generate plots displaying distribution vs
demographics:
[`addDefaultConfigForHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForHistograms.md),
[`validateDistributionVsDemographicsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateDistributionVsDemographicsConfig.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example for plotting a histogram of PK-parameters of a scenario compared to a reference scenario:
runPlot(
  nameOfplotFunction = "plotDistributionVsDemographics",
  projectConfiguration = projectConfiguration,
  configTableSheet = "DistributionVsRange",
  inputs = list(
    scenarioList = scenarioList,
    pkParameterDT = pkParameterDT,
    aggregationFlag = "GeometricStdDev"
  )
)
} # }
```
