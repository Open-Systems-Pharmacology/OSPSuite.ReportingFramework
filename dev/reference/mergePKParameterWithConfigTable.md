# Merge PK Parameters with Configuration Table

This function merges pharmacokinetic (PK) parameters with a given plot
configuration table. It processes the configuration data to ensure
compatibility with the PK parameter data and prepares the data for
visualization or further analysis.

## Usage

``` r
mergePKParameterWithConfigTable(
  onePlotConfig,
  pkParameterDT,
  colorVector = NULL,
  asRatio = FALSE
)
```

## Arguments

- onePlotConfig:

  A data.table containing plot configuration settings, including
  scenario names, PK parameters, and output path IDs.

- pkParameterDT:

  A data.table containing pharmacokinetic parameters, including scenario
  names, parameters, individual IDs, values, and output path IDs.

- colorVector:

  An optional vector specifying colors for different scenarios. If
  provided, it will be used to differentiate between reference and
  non-reference scenarios in the merged data.

- asRatio:

  A logical value indicating whether to convert values to ratios between
  reference and base scenarios. Defaults to FALSE.

## Value

A data.table containing the merged data with PK parameters and
configuration settings, including any calculated ratios if \`asRatio\`
is TRUE.
