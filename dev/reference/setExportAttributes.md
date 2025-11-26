# Set Export Attributes for Plot and Table Objects

This function prepares plot and table objects by setting their export
attributes, such as caption and footnote lines, which will be used when
exporting through the \`PlotManager\` class.

## Usage

``` r
setExportAttributes(
  object,
  caption = NULL,
  footNoteLines = NULL,
  exportArguments = NULL
)
```

## Arguments

- object:

  An object (e.g., a plot or table) for which export attributes will be
  set.

- caption:

  A character string specifying the caption for the object. If NULL, no
  caption will be set.

- footNoteLines:

  A character vector specifying footnote lines for the object. If, no
  footnote lines will be set.

- exportArguments:

  A list of additional arguments such as width or height passed on to
  \`ospsuite.plots::exportPlot\`.

## Value

The modified object with the specified export attributes set.
