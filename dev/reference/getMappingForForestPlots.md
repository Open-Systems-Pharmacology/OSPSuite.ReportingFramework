# Get Mapping for Forest Plots

This function generates a mapping for forest plots based on the provided
plot data and the specified column list. It uses the \`aes\` function
from ggplot2 to create the mapping, which includes x and y values, and
optionally includes error bars based on the plot data provided.

## Usage

``` r
getMappingForForestPlots(plotData, columnList)
```

## Arguments

- plotData:

  A data frame containing the data to be plotted. It should include
  columns for x values, y values, and optionally error values.

- columnList:

  A list containing the names of the columns to be used in the mapping.
  It should include at least \`yColumn\` for the y-axis.

## Value

A mapping object created using \`aes\` from ggplot2, which can be used
in a ggplot call to create forest plots.
