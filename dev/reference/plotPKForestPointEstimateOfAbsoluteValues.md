# PK Forest Plots

This is one of a group of functions which generates PK forest plots for
pharmacokinetic (PK) parameters or population parameter, providing
visualizations that can include absolute values or ratios, along with
options for displaying variance or point estimates. These functions
allow for the comparison of simulated and observed PK parameter data
across different scenarios, aiding in the interpretation of variability
and uncertainty in the estimates.

The aggregation of data can be customized using the \`aggregationFlag\`
parameter, which allows for options such as "GeometricStdDev",
"ArithmeticStdDev", "Percentiles", and "Custom" functions for
calculating summary statistics.

Available functions are:

- `plotPKForestAggregatedAbsoluteValues`:

  Generates a PK forest plot displaying absolute values along with
  variance.

- `plotPKForestPointEstimateOfAbsoluteValues`:

  Generates a PK forest plot displaying point estimates.

- `plotPKForestAggregatedRatios`:

  Generates a PK forest plot displaying ratios of PK parameters along
  with variance.

- `plotPKForestPointEstimateOfRatios`:

  Generates a PK forest plot displaying point estimates of ratios of PK
  parameters.

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
  digitsToRound = 2,
  digitsToShow = 2
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration object containing the necessary settings and
  file paths for the project.

- onePlotConfig:

  A data.table containing configuration settings for a single plot,
  including plot name, x-axis scale, and other aesthetic settings.

- pkParameterDT:

  A data.table containing simulated PK parameter data, including columns
  for scenario names, parameters, values, and output paths.

- dataObservedPK:

  Optional data.table containing observed PK parameter data for
  comparison, which can include columns for observed values and
  associated metadata.

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

  A numeric value between 0 and 1 indicating the desired confidence
  level for the intervals (e.g., 0.9 for 90 (for
  \`plotPKForestPointEstimateOfAbsoluteValues\` and
  \`plotPKForestPointEstimateOfRatios\`)

- nBootstrap:

  An integer indicating the number of bootstrap samples to draw for
  calculating confidence intervals. (for
  \`plotPKForestPointEstimateOfAbsoluteValues\`,
  \`plotPKForestPointEstimateOfRatios\`)

- scaleVectors:

  A list containing user-defined scale vectors for the plot aesthetics
  for simulated and observed data. The list can have up to two
  components: \`simulated\` and \`observed\`, each containing a list of
  properties (\`color\`, \`fill\`, \`shape\`) to modify the default
  scale vector settings.

- labelWrapWidth:

  Numeric value specifying the maximum width for wrapping labels in the
  plot to enhance readability.

- vlineIntercept:

  Optional numeric value indicating where to draw a vertical line on the
  plot, often used to denote a reference value.

- withTable:

  logical, if TRUE (default) values are displayed as table beside the
  plot

- relWidths:

  Optional numeric vector specifying relative widths for the plot and
  table.

- digitsToRound:

  An integer specifying the number of digits to round in the displayed
  values.

- digitsToShow:

  An integer specifying the number of digits to show in the displayed
  values.

## Value

A list containing the generated plot objects.

## Details

The function distinguishes between two cases for ratio calculations:

- Case 1::

  If the scenarios being compared are based on the **same populations**
  (same \`PopulationId\` in scenario configuration), the plots will show
  summary statistics of individual ratios.

- Case 2::

  If the scenarios are based on **different populations**, the plots
  will display the ratio of summary statistics. This ratio is only
  available for `plotPKForestPointEstimateOfRatios`

The uncertainty in estimates is calculated using a bootstrapping
approach. A unique seed is set for each combination of scenario,
reference scenario (if available), output path ID, and PK parameter
identifier to ensure reproducibility. The number of bootstrap samples is
defined by the \`nBootstrap\` parameter, and the confidence intervals
are determined using the \`confLevel\` parameter (e.g., 0.9 for 90

For the bootstrapping process, a unique seed is set for each combination
of scenario, \`referenceScenario\` (if available), \`outputPathId\`, and
\`PKParameter\` identifier. This ensures that results are consistent
when a specific combination is used in different plots and that they are
reproducible.

In addition to the point estimates and their confidence intervals, the
precision watermark is an important feature of these plots. The
precision watermark indicates whether the precision requirements for the
displayed estimates have been met. The precision is calculated based on
the relationship between the estimated values (\`xValues\`) and their
corresponding minimum (\`xMin\`) and maximum (\`xMax\`) confidence
interval bounds. If the calculated precision falls below a predefined
threshold (e.g., 0.01), the watermark is activated, displaying a warning
label such as "Outside precision requirement" on the plot. The threshold
is set by the option \`OSPSuite.RF.RequiredPrecisison\` This alert
serves to inform users that the estimates may not be reliable due to
insufficient sample sizes or high variability in the data, prompting
them to consider increasing the sample size for improved precision. If
the precision is sufficient, point estimates are displayed without
confidence interval bounds.

By including the precision watermark, users are better equipped to
assess the reliability of the point estimates and make informed
decisions based on the visualized data.

To support the creation of the configuration table a support function
`addDefaultConfigForPKForestPlots` is available

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
