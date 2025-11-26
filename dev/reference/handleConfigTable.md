# Handle Configuration Table Processing

This helper function processes the configuration table, generating plots
according to the specified configurations.

## Usage

``` r
handleConfigTable(
  rmdPlotManager,
  configTable,
  projectConfiguration,
  inputs,
  suppressExport
)
```

## Arguments

- rmdPlotManager:

  An RmdPlotManager object responsible for managing Rmd file generation
  and plot exports.

- configTable:

  A data frame containing the configuration settings for the plots.

- projectConfiguration:

  A ProjectConfiguration object containing the project configuration
  settings.

- inputs:

  A list of additional inputs for the plot function.

- suppressExport:

  A logical value indicating whether to suppress the export of the Rmd
  file.
