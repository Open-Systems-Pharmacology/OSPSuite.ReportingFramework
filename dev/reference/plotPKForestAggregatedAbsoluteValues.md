# Plot PK Forest Aggregated Absolute Values

Generates PK forest plots showing aggregated absolute values.

## Usage

``` r
plotPKForestAggregatedAbsoluteValues(
  projectConfiguration,
  onePlotConfig,
  pkParameterDT,
  dataObservedPK = NULL,
  aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles", "Custom"),
  percentiles = getOspsuite.plots.option(optionKey = OptionKeys$defaultPercentiles),
  customFunction = NULL,
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

- aggregationFlag:

  Aggregation mode for variance summaries.

- percentiles:

  Percentiles used when \`aggregationFlag\` is \`Percentiles\`.

- customFunction:

  An optional custom function for aggregation. A custom function should
  take a numeric vector \`y\` as input and return a list containing:

  \- \`yValues\`: The aggregated value (e.g., mean). - \`yMin\`: The
  lower value of the aggregated data (e.g., mean - sd). - \`yMax\`: The
  upper value of the aggregated data (e.g., mean + sd). -
  \`yErrorType\`: A string indicating the type of error associated with
  the aggregation, it is used in plot legends and captions. It must be a
  concatenation of the descriptor of \`yValues\` and the descriptor of
  \`yMin - yMax\` range separated by "\|" (e.g., "mean \| standard
  deviation" or "median \| 5th - 95th percentile").

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
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`plotSensitivity()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotSensitivity.md),
[`plotTimeProfiles()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotTimeProfiles.md)

Other functions to generate forest plots:
[`addDefaultConfigForPKForestPlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForPKForestPlots.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`validatePKForestAggregatedAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedAbsoluteValuesConfig.md),
[`validatePKForestAggregatedRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedRatiosConfig.md),
[`validatePKForestPointEstimateOfAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfAbsoluteValuesConfig.md),
[`validatePKForestPointEstimateOfRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfRatiosConfig.md)
