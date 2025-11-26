# Plot Sensitivity Function

Generates sensitivity plots based on the provided project configuration
and scenario list.

## Usage

``` r
plotSensitivity(projectConfiguration, onePlotConfig, scenarioList)
```

## Arguments

- projectConfiguration:

  A list containing project configuration settings, including file paths
  and parameters.

- onePlotConfig:

  A data frame containing the configuration for a single plot.

- scenarioList:

  A list of scenarios for which the sensitivity analysis will be
  performed.

## Value

A list of plots and tables, where each entry corresponds to a generated
sensitivity plot or its data.

## See also

Other plot functions:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md),
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md),
[`plotHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotHistograms.md),
[`plotPKBoxwhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKBoxwhisker.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)
