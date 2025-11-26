# Paste Figure Tags for Captions

This function generates a formatted caption text by combining unique
captions with associated plot tags. If all captions are the same, it
returns that caption. Otherwise, it creates a string that includes the
unique captions and their corresponding tags.

## Usage

``` r
pasteFigureTags(
  dtCaption,
  captionColumn,
  endWithDot = FALSE,
  startWithBlank = FALSE
)
```

## Arguments

- dtCaption:

  A data.table containing the captions and plot tags. It must have at
  least the following columns: - \`captionColumn\`: The column name
  containing the captions. - \`plotTag\`: A column containing the plot
  tags associated with each caption.

- captionColumn:

  A string specifying the name of the column in \`dtCaption\` that
  contains the captions.

- endWithDot:

  A logical value indicating whether to append a period at the end of
  the caption text. Default is FALSE.

- startWithBlank:

  boolean if TRUE adds as prefix a blank

## Value

A character string representing the formatted caption text, which
includes the captions and associated plot tags.
