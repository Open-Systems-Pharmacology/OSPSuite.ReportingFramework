# Run and save scenarios

This function simulates a list of scenarios and saves the results. If
results already exist for a scenario, it will overwrite them based on
the provided options.

## Usage

``` r
runAndSaveScenarios(
  projectConfiguration,
  scenarioList,
  simulationRunOptions = NULL,
  ...
)
```

## Arguments

- projectConfiguration:

  Configuration for the project, containing paths and settings necessary
  to run the simulations and save the results.

- scenarioList:

  Named list of Scenario objects to be simulated.

- simulationRunOptions:

  Object of type \`SimulationRunOptions\` that will be passed to
  simulation runs. If \`NULL\`, default options are used.

- ...:

  Additional arguments passed to \`esqlabsR::saveScenarioResults\`.

## Value

A list containing the simulation results for each scenario that was run.

## See also

Other scenario management:
[`calculatePKParameterForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/calculatePKParameterForScenarios.md),
[`createScenarios.wrapped()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createScenarios.wrapped.md),
[`loadPKParameter()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadPKParameter.md),
[`loadScenarioResultsToFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/loadScenarioResultsToFramework.md),
[`runOrLoadScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runOrLoadScenarios.md)

## Examples

``` r
if (FALSE) { # \dontrun{
runAndSaveScenarios(
  projectConfiguration = myProjectConfig,
  scenarioList = myScenarioList,
  simulationRunOptions = myRunOptions
)
} # }
```
