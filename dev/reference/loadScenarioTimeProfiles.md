# Load Scenario Time Profiles

This function aggregates simulated time profiles for different scenarios
based on the provided results and output paths.

## Usage

``` r
loadScenarioTimeProfiles(
  projectConfiguration,
  simulatedResults,
  outputPathsPerScenario,
  aggregationFun
)
```

## Arguments

- projectConfiguration:

  Object of class \`ProjectConfiguration\` containing information on
  paths and file names

- simulatedResults:

  A list containing simulation results for each scenario.

- outputPathsPerScenario:

  A named list of output paths for each scenario.

- aggregationFun:

  A function to aggregate simulation data.

## Value

A data.table containing the aggregated simulated time profiles for all
scenarios.
