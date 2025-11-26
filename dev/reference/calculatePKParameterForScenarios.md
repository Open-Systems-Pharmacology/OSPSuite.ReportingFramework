# Calculate Pharmacokinetic (PK) Parameters

This function calculates pharmacokinetic (PK) parameters for specified
scenarios based on project configuration and simulation results. It
generates output files for each scenario in the designated output
folder.

## Usage

``` r
calculatePKParameterForScenarios(projectConfiguration, scenarioResults)
```

## Arguments

- projectConfiguration:

  A list containing project configuration settings, including the output
  folder path and the PK parameter file.

- scenarioResults:

  A named list of scenario results, each containing simulation data for
  PK analysis.

## Value

This function is called for its side effects and does not return a
value.

## See also

Other scenario management:
[`createScenarios.wrapped()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createScenarios.wrapped.md),
[`loadPKParameter()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadPKParameter.md),
[`loadScenarioResultsToFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadScenarioResultsToFramework.md),
[`runAndSaveScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runAndSaveScenarios.md),
[`runOrLoadScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runOrLoadScenarios.md)
