# Finalize Caption Table

This function finalizes the caption table by updating plot IDs,
generating plot tags, and merging with output paths to create a
comprehensive data table for plotting.

## Usage

``` r
finalizeCaptionTable(
  dtCaption,
  timeTags,
  dtOutputPaths,
  nFacetColumns,
  nMaxFacetRows
)
```

## Arguments

- dtCaption:

  A data.table containing the initial caption data with at least a
  \`plotId\` column.

- timeTags:

  A data.table with time tags and corresponding captions, must include
  \`tag\` and \`captionText\` columns.

- dtOutputPaths:

  A data.table containing output paths with at least \`outputPathId\`
  and \`displayName\` columns.

- nFacetColumns:

  An integer specifying the number of facet columns, defaults to NULL.

- nMaxFacetRows:

  An integer specifying the maximum number of facet rows.

## Value

A data.table that includes updated plot IDs, plot tags, and merged
output display names.
