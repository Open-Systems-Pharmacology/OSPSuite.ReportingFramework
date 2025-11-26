# Run Sensitivity Analysis for Specified Scenarios

This function executes sensitivity analysis for the specified scenarios
in the project configuration. It checks for the presence of the
sensitivity file and runs the analysis, saving the results to CSV files.

## Usage

``` r
runSensitivityAnalysisForScenarios(
  projectConfiguration,
  scenarioList,
  scenarioNames = NULL,
  sensitivitysheet,
  sensitivityAnalysisRunOptions =
    ospsuite::SensitivityAnalysisRunOptions$new(showProgress = TRUE),
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

- sensitivityAnalysisRunOptions:

  Options for running the sensitivity analysis. Defaults to an instance
  of \`SensitivityAnalysisRunOptions\` with \`showProgress\` set to
  TRUE.

- overwrite:

  A logical indicating whether to overwrite existing results. Defaults
  to TRUE.
