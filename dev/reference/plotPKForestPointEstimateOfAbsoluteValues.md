# Plot PK Forest Point Estimate of Absolute Values

Generates PK forest plots showing point estimates of absolute values.

## Usage

``` r
plotPKForestPointEstimateOfAbsoluteValues(
  projectConfiguration,
  onePlotConfig,
  pkParameterDT,
  dataObservedPK = NULL,
  statFun = c(`geometric mean` = function(y) exp(mean(log(y[y > 0])))),
  confLevel = 0.9,
  nBootstrap = 100,
  scaleVectors = list(),
  labelWrapWidth = 10,
  vlineIntercept = NULL,
  withTable = TRUE,
  relWidths = NULL,
  heightToWidth = 1,
  digitsToRound = 2,
  digitsToShow = 2
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration object containing report and scenario settings.

- onePlotConfig:

  A data.table with plot configuration rows for one plot definition.

- pkParameterDT:

  A data.table containing simulated PK parameter values.

- dataObservedPK:

  Optional data.table containing observed PK parameter values.

- statFun:

  A named list of functions used for statistical aggregation, typically
  including the geometric mean function for bootstrapping. This must be
  a function accepting as input a numeric vector and returning a numeric
  value. The input is formatted as a named list, the name is used in
  plot legends and captions c('geometric mean' = function(y)
  exp(mean(log(y\[y\>0\])))) (for
  \`plotPKForestPointEstimateOfAbsoluteValues\` and
  \`plotPKForestPointEstimateOfRatios\`)

- confLevel:

  Confidence level used for bootstrap intervals.

- nBootstrap:

  Number of bootstrap samples.

- scaleVectors:

  Optional scale overrides for simulated/observed aesthetics.

- labelWrapWidth:

  Width used when wrapping long axis labels.

- vlineIntercept:

  Optional x-position(s) for vertical reference line(s).

- withTable:

  Logical indicating whether to include the side table.

- relWidths:

  Optional relative widths for plot and table layout.

- heightToWidth:

  Height-to-width ratio used for export.

- digitsToRound:

  Number of digits for numeric rounding.

- digitsToShow:

  Number of digits to show in formatted labels/tables.

## Value

A named list of plot objects keyed by plot and scale.

## See also

Other plot functions:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md),
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md),
[`plotHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotHistograms.md),
[`plotPKBoxwhisker()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKBoxwhisker.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotSensitivity()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotSensitivity.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)

Other functions to generate forest plots:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`validatePKForestAggregatedAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedAbsoluteValuesConfig.md),
[`validatePKForestAggregatedRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedRatiosConfig.md),
[`validatePKForestPointEstimateOfAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfAbsoluteValuesConfig.md),
[`validatePKForestPointEstimateOfRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfRatiosConfig.md)
