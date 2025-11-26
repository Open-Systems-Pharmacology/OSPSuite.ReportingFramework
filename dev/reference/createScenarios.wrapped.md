# Create Scenario objects from \`ScenarioConfiguration\` objects

wrap of \`esqlabsR::createDefaultProjectConfiguration()\` with
\`esqlabsR::createScenarios()\` as input

## Usage

``` r
createScenarios.wrapped(projectConfiguration, scenarioNames = NULL)
```

## Arguments

- projectConfiguration:

  Object of class \`ProjectConfiguration\` containing information on
  paths and file names

- scenarioNames:

  Names of the scenarios that are defined in the excel file. If NULL
  (default), all scenarios specified in the excel file will be created.

## Value

Named list of Scenario objects.

## See also

Other scenario management:
[`calculatePKParameterForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/calculatePKParameterForScenarios.md),
[`loadPKParameter()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadPKParameter.md),
[`loadScenarioResultsToFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadScenarioResultsToFramework.md),
[`runAndSaveScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runAndSaveScenarios.md),
[`runOrLoadScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runOrLoadScenarios.md)
