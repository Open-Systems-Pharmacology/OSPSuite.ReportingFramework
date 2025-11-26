# Validation of config table for time profiles plots

Validation of config table for time profiles plots

## Usage

``` r
validateTimeProfilesConfig(
  configTable,
  dataObserved = NULL,
  scenarioResults,
  ...
)
```

## Arguments

- configTable:

  Plot configuration table.

- dataObserved:

  A \`data.table\` (formatted as produced by
  \`readObservedDataByDictionary\`) or \`DataCombined\` object
  containing the observed data to be plotted.

- scenarioResults:

  A list containing simulated scenario results.

- ...:

  Additional arguments passed to \`ospsuite.plots::plotTimeprofile\`.

## See also

Other plot configuration validation function:
[`validateDistributionVsDemographicsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateDistributionVsDemographicsConfig.md),
[`validateHistogramsConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateHistogramsConfig.md),
[`validatePKBoxwhiskerConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKBoxwhiskerConfig.md),
[`validatePKForestAggregatedAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedAbsoluteValuesConfig.md),
[`validatePKForestAggregatedRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestAggregatedRatiosConfig.md),
[`validatePKForestPointEstimateOfAbsoluteValuesConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfAbsoluteValuesConfig.md),
[`validatePKForestPointEstimateOfRatiosConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validatePKForestPointEstimateOfRatiosConfig.md),
[`validateSensitivityConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateSensitivityConfig.md)
