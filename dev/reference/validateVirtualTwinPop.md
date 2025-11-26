# Validate Virtual Twin Population in Plot Configuration

This function validates the virtual twin population specified in the
plot configuration table. It checks for the presence of
\`individualIds\` in scenarios with virtual twin populations, ensures
that brackets are not used in \`individualIds\` for time profile plots,
and warns if \`individualIds\` is filled without a corresponding data
group.

## Usage

``` r
validateVirtualTwinPop(configTablePlots, scenarioResults)
```

## Arguments

- configTablePlots:

  A data table containing the plot configuration, including scenario
  names and individual IDs.

- scenarioResults:

  A list with scenario results.
