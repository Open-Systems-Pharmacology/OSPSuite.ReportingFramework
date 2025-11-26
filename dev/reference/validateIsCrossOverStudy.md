# Validate Cross-Over Study

This function checks if the provided \`configTablePlots\` and
\`pkParameterDT\` data frames represent a valid cross-over study by
ensuring that the scenario and reference scenario are based on the same
population.

## Usage

``` r
validateIsCrossOverStudy(configTablePlots, pkParameterDT)
```

## Arguments

- configTablePlots:

  A data.table containing configuration for plots, including scenario
  and referenceScenario columns.

- pkParameterDT:

  A data.table containing pharmacokinetic parameters including scenario
  names and population IDs.

## Value

None. The function will print any invalid configurations and stop
execution if the validation fails.
