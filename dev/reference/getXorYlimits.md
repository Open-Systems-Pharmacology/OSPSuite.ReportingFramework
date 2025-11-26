# Get Y-axis Limits for Plotting

This function retrieves the y-axis limits from a given plot
configuration based on the specified y-axis scale (linear or
logarithmic). It also allows for additional arguments to be passed for
further customization.

## Usage

``` r
getXorYlimits(onePlotConfig, xOryScale, direction = c("y", "x"), ...)
```

## Arguments

- onePlotConfig:

  A list containing the plot configuration, which must include
  \`ylimit_linear\` and \`ylimit_log\` elements if direction is \`y\`
  otherwise \`xlimit_linear\` and \`xlimit_log\`

- xOryScale:

  A character string indicating the type of scale to use for the
  y(x)-axis. It should be either "linear" or "log".

- direction:

  A character string indicating direction of the axis must be \`x\` or
  \`y\` default is \`y\`.

- ...:

  Additional arguments that can be passed to customize the y-scale.
  These can include \`yscale.args\` for further customization of the
  y-axis scale.

## Value

A list of y-scale arguments, including limits, which can be used in
plotting functions.
