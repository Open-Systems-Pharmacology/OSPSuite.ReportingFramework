# Get Default Colors for Scale Vector

This function generates a vector of default colors based on the
specified shade and number of colors required. It provides colors from
the ggsci package for small numbers and a custom color map for larger
requests.

## Usage

``` r
getDefaultColorsForScaleVector(shade = c("dark", "light"), n)
```

## Arguments

- shade:

  A character string indicating the shade of colors to return. Must be
  either "dark" or "light". Default is "dark".

- n:

  An integer specifying the number of colors to return. Must be greater
  than or equal to 1.

## Value

A character vector of color values in hexadecimal format.

## Details

\- For \`n\` values less than or equal to 10, the function uses the
ggsci package's "category20c" palette. - For \`n\` values greater than
10, it retrieves colors from the predefined color map of the package
\`ospsuite.plots\` named "ospDefault". - If \`n\` exceeds the maximum
number of colors available in "ospDefault", an error is raised.
