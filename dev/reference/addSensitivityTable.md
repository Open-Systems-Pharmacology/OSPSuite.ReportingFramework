# Add Sensitivity Table to Project Configuration

This function adds a sensitivity table to the provided project
configuration. It loads a template Excel file and, when \`scenarioList\`
is provided, populates it with parameter paths derived across the
supplied scenarios.

## Usage

``` r
addSensitivityTable(
  projectConfiguration,
  scenarioList = NULL,
  sheetName = "Sensitivity",
  parameterPathMode = c("intersection", "union")
)
```

## Arguments

- projectConfiguration:

  An object representing the project configuration.

- scenarioList:

  A named list of scenarios from which to extract parameter paths. If
  \`NULL\`, only the template workbook is added to the project
  configuration.

- sheetName:

  The name of the sheet in the Excel file to populate.

- parameterPathMode:

  Character string indicating how to combine the potential parameter
  paths from \`scenarioList\`: \`"intersection"\` keeps only paths
  present in every scenario, while \`"union"\` keeps any path present in
  at least one scenario.

## Value

The modified project configuration object.

## See also

Other sensitivity analysis:
[`runRepresentativeSensitivityAnalysisForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runRepresentativeSensitivityAnalysisForScenarios.md),
[`runSensitivityAnalysisForScenarios()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runSensitivityAnalysisForScenarios.md)
