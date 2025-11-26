# Setup Virtual Twin Population Configuration

This function sets up a configuration sheet for virtual twin populations
based on the provided project configuration and observed data.

## Usage

``` r
setupVirtualTwinPopConfig(projectConfiguration, dataObserved, groups = NULL)
```

## Arguments

- projectConfiguration:

  A list containing project configuration details, including file paths
  for populations and scenarios.

- dataObserved:

  A data object containing observed data, which may be of class
  "DataCombined".

- groups:

  A character vector of group names to filter the virtual twin
  population. Defaults to NULL.

## Value

Returns NULL invisibly if no groups are available for virtual twin
population creation. Modifies the workbook specified in
projectConfiguration.

## See also

Other population export:
[`exportRandomPopulations()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportRandomPopulations.md),
[`exportVirtualTwinPopulations()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportVirtualTwinPopulations.md)
