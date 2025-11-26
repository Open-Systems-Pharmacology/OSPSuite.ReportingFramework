# Generate Population Files

This function generates population files based on individual biometrics
and parameters.

## Usage

``` r
.generatePopulationFiles(
  dtTwinPops,
  params,
  dtIndividualBiometrics,
  projectConfiguration,
  sim
)
```

## Arguments

- dtTwinPops:

  A data.table containing virtual twin population data.

- params:

  A list of parameters for the virtual twin population.

- dtIndividualBiometrics:

  A data.table containing individual biometrics.

- projectConfiguration:

  A list containing project configuration details.

- sim:

  A simulation object.
