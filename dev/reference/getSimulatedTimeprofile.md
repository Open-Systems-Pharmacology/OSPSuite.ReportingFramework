# Get Simulated Time Profile

This function processes simulation results into a data table format for
time profiles.

## Usage

``` r
getSimulatedTimeprofile(
  simulatedResult,
  outputPaths,
  aggregationFun,
  individualMatch
)
```

## Arguments

- simulatedResult:

  The simulation results to be processed.

- outputPaths:

  A vector of output paths to be included in the time profile.

- aggregationFun:

  A function to aggregate the simulation data if necessary.

- individualMatch:

  data.table with matches simulated individual id with individual id of
  observed data, is only filled for individual populations otherwise
  NULL

## Value

A data.table with the processed time profile data.
