# Construct Metadata for Time Profile

This function generates metadata for time profile plots based on the
provided plot data. It extracts and validates the dimensions and units
for both x and y values, ensuring that there is no ambiguity in the
units.

## Usage

``` r
.constructMetDataForTimeProfile(plotData)
```

## Arguments

- plotData:

  A data.table containing the following relevant columns: - \`xUnit\`:
  The unit of the x-axis values. - \`xDimension\`: (optional) The
  dimension of the x values, if already specified. - \`yUnit\`: The
  unit(s) of the y-axis values (can be one or two units). -
  \`yDimension\`: (optional) The dimension of the y values, if already
  specified.

## Value

A list containing metadata with the following structure: - \`xValues\`:
A list with \`dimension\` and \`unit\` for the x-axis. - \`yValues\`: A
list with \`dimension\` and \`unit\` for the primary y-axis. - \`y2\`:
(optional) A list with \`dimension\` and \`unit\` for the secondary
y-axis if applicable.

## Details

The function checks for ambiguities in the x and y units and retrieves
the corresponding dimensions. If two y units are provided, it constructs
separate metadata for each. The resulting metadata is returned as a
list, which includes dimensions and units for both x and y values.
