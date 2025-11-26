# Plot Histograms

Generates histogram plots based on the provided configuration and data.

## Usage

``` r
plotHistograms(
  projectConfiguration,
  onePlotConfig,
  pkParameterDT = NULL,
  scenarioList = NULL,
  facetAspectRatio = 0.5,
  colorVector = c(scenario = NA, referenceScenario = NA),
  nMaxFacetRows = 2,
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

- facetAspectRatio:

  A numeric value for the aspect ratio of the facets (default is 0.5).

- colorVector:

  A named vector for colors corresponding to scenarios (default is
  c(scenario = NA, referenceScenario = NA)).

- nMaxFacetRows:

  Maximum number of facet rows (default is 2).

- ...:

  Additional arguments passed to the histogram plotting function.

## Value

A list of histogram plot objects generated based on the input
configuration.

## Details

This function is intended to be used in combination with the \`runPlot\`
function. Refer to the vignette \`Plot and Report Generation\` for more
details. It can be configured to plot PK parameters or model parameters.

A helper function, \`addDefaultConfigForHistograms\`, is available to
streamline the setup of your Excel configuration sheet for histograms.
This function automatically populates a new sheet with default values
that can be customized as needed.

## See also

Other plot functions:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md),
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md),
[`plotPKBoxwhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKBoxwhisker.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotSensitivity()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotSensitivity.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)

Other functions to generate histograms:
[`addDefaultConfigForDistributionsVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForDistributionsVsDemographics.md),
[`validateHistogramsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateHistogramsConfig.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example for plotting a histogram of PK-parameters of a scenario compared to a reference scenario:
runPlot(
  nameOfplotFunction = "plotHistograms",
  projectConfiguration = projectConfiguration,
  configTableSheet = "HistogramTest",
  inputs = list(
    scenarioList = scenarioList,
    pkParameterDT = pkParameterDT,
    colorVector = c(
      "PO application" = "red",
      "IV application" = "green"
    )
  )
)
} # }
```
