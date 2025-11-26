# Generate Virtual Twin Population

This function generates virtual twin populations based on the provided
model and project configuration.

## Usage

``` r
exportVirtualTwinPopulations(
  projectConfiguration,
  modelFile,
  overwrite = FALSE,
  populationNames = NULL
)
```

## Arguments

- projectConfiguration:

  A list containing project configuration details, including file paths
  for populations and scenarios.

- modelFile:

  A string representing the name of the model file to be loaded. This
  file is used for unit conversion

- overwrite:

  A logical indicating whether to overwrite existing files. Defaults to
  FALSE

- populationNames:

  a character vector defining the population which should be exported.
  If NULL (default) all will be exported

## See also

Other population export:
[`exportRandomPopulations()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportRandomPopulations.md),
[`setupVirtualTwinPopConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setupVirtualTwinPopConfig.md)
