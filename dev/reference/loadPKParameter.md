# Load Pharmacokinetic (PK) Parameters for Specified Scenarios

This function loads pharmacokinetic (PK) parameters for specified
scenarios based on project configuration and a list of scenarios. It
processes each scenario and returns the results as a data.table.

## Usage

``` r
loadPKParameter(projectConfiguration, scenarioListOrResult)
```

## Arguments

- projectConfiguration:

  A list containing project configuration settings, including the PK
  parameter file.

- scenarioListOrResult:

  A named list of scenarios for which PK parameters are to be loaded.

## Value

A data.table containing the processed PK analyses for all specified
scenarios.

A data.table containing the processed PK analyses.

## See also

Other scenario management:
[`calculatePKParameterForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/calculatePKParameterForScenarios.md),
[`createScenarios.wrapped()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createScenarios.wrapped.md),
[`loadScenarioResultsToFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadScenarioResultsToFramework.md),
[`runAndSaveScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runAndSaveScenarios.md),
[`runOrLoadScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runOrLoadScenarios.md)
