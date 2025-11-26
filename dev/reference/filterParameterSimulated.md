# Filter Simulated Parameters

Filters simulated PK parameters based on the provided configuration.

## Usage

``` r
filterParameterSimulated(
  projectConfiguration,
  pkParameterDT,
  onePlotConfig,
  ratioMode,
  coefficientOfVariation,
  asPointeEstimate
)
```

## Arguments

- projectConfiguration:

  A ProjectConfiguration object containing the necessary settings and
  file paths for the project.

- pkParameterDT:

  A data.table containing simulated PK parameter data, including columns
  for scenario names, parameters, values, and output paths.

- onePlotConfig:

  A data.table containing configuration settings for a single plot,
  including plot name, x-axis scale, and other aesthetic settings.

- ratioMode:

  Mode for ratio calculations.

- asPointeEstimate:

  Logical indicating if confidence intervals should be calculated.

## Value

Filtered data table of simulated PK parameters.
