# Compute the Ratio of Values Between Base and Reference Scenarios

This function calculates the ratio of values between base and reference
scenarios for specified pharmacokinetic parameters. It merges the
configuration data with pharmacokinetic parameter data and computes the
ratio based on individual IDs and output path IDs.

## Usage

``` r
setValueToRatio(mergedData, pkParameterDT)
```

## Arguments

- mergedData:

  A data.table containing configuration data with columns for
  referenceScenario, pkParameter, individualId, and outputPathId.

- pkParameterDT:

  A data.table containing pharmacokinetic parameter data including
  scenario names, parameters, individual IDs, output path IDs, values,
  and population IDs.

## Value

A data.table containing the merged data with a new column \`value\`
representing the ratio of base to reference values.
