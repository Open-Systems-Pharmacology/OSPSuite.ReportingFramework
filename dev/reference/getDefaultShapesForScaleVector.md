# Get Default Shapes for Scale Vector

This function retrieves a vector of default shapes for plotting based on
the specified number of shapes required. It utilizes shape settings from
the ospsuite.plots package.

## Usage

``` r
getDefaultShapesForScaleVector(n)
```

## Arguments

- n:

  An integer specifying the number of shapes to return. Must be greater
  than or equal to 1.

## Value

A character vector of shape values.

## Details

\- The function calls \`getOspsuite.plots.option\` to obtain the default
shape values. - If no shapes are available, an error is raised,
prompting the user to set defaults using
\`ospsuite.plots::setDefaults()\`. - If the requested number of shapes
exceeds the available shapes, an error is raised.
