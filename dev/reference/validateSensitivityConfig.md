# Validate Sensitivity Configuration Table

This function checks the headers of the configuration table, validates
output IDs and data group IDs for plotting, and ensures the
configuration adheres to specified criteria. It also checks for file
existence in the output folder.

## Usage

``` r
validateSensitivityConfig(configTable, ...)
```

## Arguments

- configTable:

  A data.table containing the configuration for sensitivity analysis.

- ...:

  Additional arguments passed to other functions.

## Value

A validated data frame containing the configuration table for
sensitivity plots.

## See also

Other plot configuration validation function:
[`validateDistributionVsDemographicsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateDistributionVsDemographicsConfig.md),
[`validateHistogramsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateHistogramsConfig.md),
[`validatePKBoxwhiskerConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKBoxwhiskerConfig.md),
[`validatePKForestAggregatedAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedAbsoluteValuesConfig.md),
[`validatePKForestAggregatedRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedRatiosConfig.md),
[`validatePKForestPointEstimateOfAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfAbsoluteValuesConfig.md),
[`validatePKForestPointEstimateOfRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfRatiosConfig.md),
[`validateTimeProfilesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateTimeProfilesConfig.md)
