# Generate Histograms

Generates histogram plots based on the provided configuration and data.

## Usage

``` r
generateHistograms(
  onePlotConfig,
  plotData,
  colorVector,
  facetAspectRatio,
  nMaxFacetRows,
  ...
)
```

## Arguments

- onePlotConfig:

  A configuration for the specific plot.

- plotData:

  A data.table containing the plot data.

- colorVector:

  A named vector for colors corresponding to scenarios.

- facetAspectRatio:

  A numeric value for the aspect ratio of the facets.

- nMaxFacetRows:

  Maximum number of facet rows.

- ...:

  Additional arguments passed to the histogram plotting function.

## Value

A list of histogram plot objects.
