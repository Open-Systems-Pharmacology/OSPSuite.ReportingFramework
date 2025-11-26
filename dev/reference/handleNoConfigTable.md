# Handle Case with No Configuration Table

This helper function processes the plotfunctions where no configuration
table is provided, generating plots based on default settings and
exporting them.

## Usage

``` r
handleNoConfigTable(
  rmdPlotManager,
  projectConfiguration,
  inputs,
  suppressExport
)
```

## Arguments

- rmdPlotManager:

  An RmdPlotManager object responsible for managing Rmd file generation
  and plot exports.

- projectConfiguration:

  A ProjectConfiguration object containing the project configuration
  settings.

- inputs:

  A list of additional inputs for the plot function.

- suppressExport:

  A logical value indicating whether to suppress the export of the Rmd
  file.
