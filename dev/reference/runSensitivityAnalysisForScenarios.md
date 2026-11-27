# Run Sensitivity Analysis for Specified Scenarios

This function executes sensitivity analysis for the specified scenarios
in the project configuration. It checks for the presence of the
sensitivity file, derives the PK parameters and output paths configured
for each scenario, and runs the enhanced sensitivity workflow, saving
the results to CSV files.

## Usage

``` r
runSensitivityAnalysisForScenarios(
  projectConfiguration,
  scenarioList,
  scenarioNames = NULL,
  sensitivitysheet,
  referenceScenario = NULL,
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

  A list of scenarios to analyze.

- scenarioNames:

  A vector of scenario names to run the analysis on. Defaults to NULL.

- sensitivitysheet:

  The name of the sheet in the sensitivity Excel file to use for
  analysis.

- referenceScenario:

  Optional scenario name used as the denominator for ratio mode. When
  provided, sensitivities are computed as \`scenario /
  referenceScenario\`.

- variationRange:

  Numeric fraction defining how much each sensitivity parameter is
  varied around baseline (e.g., \`0.1\` means +/- 10%).

- numberOfSteps:

  Integer number of perturbation steps on each side of baseline used for
  sensitivity estimation.

- simulationRunOptions:

  Optional \`SimulationRunOptions\` passed to the simulation runs.

- overwrite:

  A logical indicating whether to overwrite existing results. Defaults
  to TRUE.

## Details

Repeated rows in the sensitivity sheet that share the same \`parameter\`
value are treated as one grouped sensitivity label, so multiple
\`parameterPath\` values can be varied together.

Ratio mode is only applied when \`referenceScenario\` is provided. In
that case the sensitivity is calculated for \`scenario /
referenceScenario\`.

Runs sensitivity analysis for each selected scenario and optionally
computes scenario/reference ratios when \`referenceScenario\` is
provided.

## See also

Other sensitivity analysis:
[`addSensitivityTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addSensitivityTable.md),
[`runRepresentativeSensitivityAnalysisForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runRepresentativeSensitivityAnalysisForScenarios.md)
