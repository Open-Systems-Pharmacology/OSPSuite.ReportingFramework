# Run representative-individual sensitivity analysis for population scenarios

This function selects representative individuals from a scenario
population based on configured PK parameters and requested quantiles,
then runs the existing sensitivity workflow separately for each
representative.

## Usage

``` r
runRepresentativeSensitivityAnalysisForScenarios(
  projectConfiguration,
  scenarioList,
  scenarioNames = NULL,
  sensitivitysheet,
  representativeQuantiles = c(0.05, 0.5, 0.95),
  variationRange = 0.1,
  numberOfSteps = 2,
  simulationRunOptions = ospsuite::SimulationRunOptions$new(showProgress = TRUE),
  overwrite = TRUE
)
```

## Arguments

- projectConfiguration:

  An object representing the project configuration.

- scenarioList:

  A named list of scenarios to analyze.

- scenarioNames:

  A character vector of scenario names to run. Defaults to \`NULL\`,
  which analyzes all scenarios in \`scenarioList\`.

- sensitivitysheet:

  The name of the sheet in the sensitivity Excel file to use for
  analysis.

- representativeQuantiles:

  Numeric vector of quantiles used to select representative individuals
  per PK parameter and output path. Defaults to \`c(0.05, 0.5, 0.95)\`.

- variationRange:

  Numeric fraction defining how much each sensitivity parameter is
  varied around baseline (e.g., \`0.1\` means +/- 10%).

- numberOfSteps:

  Integer number of perturbation steps on each side of baseline used for
  sensitivity estimation.

- simulationRunOptions:

  Optional \`SimulationRunOptions\` passed to the simulation runs and
  prerequisite scenario simulations.

- overwrite:

  A logical indicating whether to overwrite existing results.

## Value

Invisibly returns \`NULL\`.

## Details

Ratio mode is intentionally not supported for this workflow.

## See also

Other sensitivity analysis:
[`addSensitivityTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addSensitivityTable.md),
[`runSensitivityAnalysisForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runSensitivityAnalysisForScenarios.md)
