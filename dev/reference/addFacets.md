# Add Facets to a ggplot Object

This function adds facets to a given ggplot object, allowing for better
visualization of data subsets. Faceting is done by the variable
\`plotTag\`.

## Usage

``` r
addFacets(plotObject, facetScale, facetAspectRatio = 0.5, nFacetColumns)
```

## Arguments

- plotObject:

  A ggplot object to which the facets should be added.

- facetScale:

  A character string indicating the scale of the facets. Options are
  "free", "fixed", "free_x", or "free_y".

- facetAspectRatio:

  A numeric value specifying the aspect ratio of the facets. Default is
  0.5.

- nFacetColumns:

  An integer specifying the number of columns to use for the facet
  layout. If NULL, no faceting is done.

## Value

An updated ggplot object with facets added.
