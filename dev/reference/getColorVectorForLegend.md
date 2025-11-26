# Get Color Vector for Legend

This function generates a color vector for a given color legend, using a
specified color vector to modify default colors. The color legend is
expected to be a character string containing color names separated by a
pipe (\`\|\`). The function first validates the provided color vector,
then generates default colors, and applies any colors from the specified
color vector.

## Usage

``` r
getColorVectorForLegend(colorLegend, colorVector)
```

## Arguments

- colorLegend:

  A character string containing color names separated by a pipe
  (\`\|\`).

- colorVector:

  A named character vector where names correspond to color names in
  \`colorLegend\`. This vector will be used to modify the default color
  values.

## Value

A named character vector of colors corresponding to the provided color
legend.
