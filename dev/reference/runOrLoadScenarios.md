# Run or load scenarios

This function checks if the simulation results for scenarios already
exist. If they do, it loads them; otherwise, it runs the scenarios and
saves the results.

## Usage

``` r
runOrLoadScenarios(
  projectConfiguration,
  scenarioList,
  simulationRunOptions = NULL,
  ...
)
```

## Arguments

- projectConfiguration:

  Configuration for the project, containing paths and settings necessary
  to run the simulations and load the results.

- scenarioList:

  Named list of Scenario objects to be managed.

- simulationRunOptions:

  Object of type \`SimulationRunOptions\` that will be passed to
  simulation runs. If \`NULL\`, default options are used.

- ...:

  Additional arguments passed to \`runAndSaveScenarios\`.

## Value

A list containing the simulation results for each scenario that was
loaded or run.

## See also

Other scenario management:
[`calculatePKParameterForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/calculatePKParameterForScenarios.md),
[`createScenarios.wrapped()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createScenarios.wrapped.md),
[`loadPKParameter()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadPKParameter.md),
[`loadScenarioResultsToFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadScenarioResultsToFramework.md),
[`runAndSaveScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runAndSaveScenarios.md)
