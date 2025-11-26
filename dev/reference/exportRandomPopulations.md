# Export Random Populations

This function generates virtual populations based on demographic data
and exports them to CSV files.

## Usage

``` r
exportRandomPopulations(
  projectConfiguration,
  populationNames = NULL,
  customParameters = NULL,
  overwrite = FALSE
)
```

## Arguments

- projectConfiguration:

  A list containing project configuration details, including: -
  populationsFile: Path to the Excel file containing population
  demographics. - populationsFolder: Directory where the generated CSV
  files will be saved.

- populationNames:

  A character vector of population names to generate. If NULL, all
  populations in the demographics sheet will be considered.

- customParameters:

  A list of custom parameters to set for the populations. Each item in
  the list should be a list containing: - path: The parameter path to
  set. - values: A vector of values to assign to the parameter.

- overwrite:

  A logical indicating whether to overwrite existing population files.
  Default is FALSE.

## Value

This function does not return a value. It generates and exports CSV
files for the specified populations.

## See also

Other population export:
[`exportVirtualTwinPopulations()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportVirtualTwinPopulations.md),
[`setupVirtualTwinPopConfig()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setupVirtualTwinPopConfig.md)

## Examples

``` r
if (FALSE) { # \dontrun{
exportRandomPopulations(projectConfiguration,
  populationNames = c("Population1", "Population2"),
  customParameters = list(list(path = "param1", values = c(1, 2))),
  overwrite = TRUE
)
} # }
```
