# Add Default Configuration for PK Forest Plots

Adds default configurations for forest plots to the \`Plots.xlsx\`
configuration file.

## Usage

``` r
addDefaultConfigForPKForestPlots(
  projectConfiguration,
  pkParameterDT,
  sheetName = "PKParameter_Forest",
  overwrite = FALSE
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration object.

- pkParameterDT:

  A data.table containing PK parameter data.

- sheetName:

  Name of the sheet to create.

- overwrite:

  Logical indicating if existing data should be overwritten.

## Value

NULL (invisible).

## See also

[`plotPKForestAggregatedAbsoluteValues`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestPointEstimateOfAbsoluteValues`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestAggregatedRatios`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfRatios`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),

Other plot functions:
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md),
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
[`addDefaultConfigForTimeProfilePlots()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForTimeProfilePlots.md)

Other functions to generate forest plots:
[`plotPKForestAggregatedAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedAbsoluteValues.md),
[`plotPKForestAggregatedRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestAggregatedRatios.md),
[`plotPKForestPointEstimateOfAbsoluteValues()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfAbsoluteValues.md),
[`plotPKForestPointEstimateOfRatios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotPKForestPointEstimateOfRatios.md),
[`validatePKForestAggregatedAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedAbsoluteValuesConfig.md),
[`validatePKForestAggregatedRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedRatiosConfig.md),
[`validatePKForestPointEstimateOfAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfAbsoluteValuesConfig.md),
[`validatePKForestPointEstimateOfRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfRatiosConfig.md)
