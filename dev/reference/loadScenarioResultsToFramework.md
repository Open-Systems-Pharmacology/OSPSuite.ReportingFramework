# Load existing scenario results

This function loads the results of specified scenarios. If the results
do not exist, it returns an error.

## Usage

``` r
loadScenarioResultsToFramework(projectConfiguration, scenarioNames)
```

## Arguments

- projectConfiguration:

  Configuration for the project, containing paths and settings necessary
  to load the results.

- scenarioNames:

  Character vector of the names of the scenarios whose results are to be
  loaded.

## Value

A list containing the loaded scenario results, including population data
if available. throws Error if the scenario results do not exist.

## See also

Other scenario management:
[`calculatePKParameterForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/calculatePKParameterForScenarios.md),
[`createScenarios.wrapped()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createScenarios.wrapped.md),
[`loadPKParameter()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadPKParameter.md),
[`runAndSaveScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runAndSaveScenarios.md),
[`runOrLoadScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runOrLoadScenarios.md)
