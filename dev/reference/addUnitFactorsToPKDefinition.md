# Process PK Parameter Definitions for a Scenario

This function processes the PK parameter definitions for a specific
scenario, including unit conversions and output filters. It ensures that
the definitions are aligned with the scenario outputs.

## Usage

``` r
addUnitFactorsToPKDefinition(
  scenarioSimulation,
  dtOutputPaths,
  dtPkAnalyses,
  dtPkParameterDefinition
)
```

## Arguments

- scenarioSimulation:

  A simulation object corresponding to the scenario.

- dtOutputPaths:

  A data.table containing output paths relevant to the scenario.

- dtPkAnalyses:

  A data.table containing PK analyses to be processed.

- dtPkParameterDefinition:

  A data.table containing definitions of PK parameters.

## Value

A data.table containing processed PK parameter definitions for the
scenario.
