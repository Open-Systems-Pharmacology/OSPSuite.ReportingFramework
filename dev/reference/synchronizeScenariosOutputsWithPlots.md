# Synchronize Scenario Outputs with Plot Outputs

This function synchronizes output paths between two Excel files: one
containing scenario outputs and the other containing plot outputs. It
only writes back to the Excel files if changes are detected in the
output paths.

## Usage

``` r
synchronizeScenariosOutputsWithPlots(
  projectConfiguration,
  direction = c("bothways", "scenarioToPlot", "plotToScenario")
)
```

## Arguments

- projectConfiguration:

  An object of class ProjectConfiguration containing the file paths for
  scenariosFile and plotsFile.

## Value

Returns invisibly.
