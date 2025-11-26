# Validate Distribution vs Demographics Configuration

Validates the configuration table for distribution vs demographics
plots.

## Usage

``` r
validateDistributionVsDemographicsConfig(configTable, scenarioList, ...)
```

## Arguments

- configTable:

  A data.table containing the configuration table.

- scenarioList:

  List of scenarios

- ...:

  Additional arguments for validation.

## Value

NULL (invisible).

## See also

Other plot configuration validation function:
[`validateHistogramsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateHistogramsConfig.md),
[`validatePKBoxwhiskerConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKBoxwhiskerConfig.md),
[`validatePKForestAggregatedAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedAbsoluteValuesConfig.md),
[`validatePKForestAggregatedRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedRatiosConfig.md),
[`validatePKForestPointEstimateOfAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfAbsoluteValuesConfig.md),
[`validatePKForestPointEstimateOfRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfRatiosConfig.md),
[`validateSensitivityConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateSensitivityConfig.md),
[`validateTimeProfilesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateTimeProfilesConfig.md)

Other functions to generate plots displaying distribution vs
demographics:
[`addDefaultConfigForHistograms()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addDefaultConfigForHistograms.md),
[`plotDistributionVsDemographics()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/plotDistributionVsDemographics.md)
