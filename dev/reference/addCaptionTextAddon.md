# Add Text to caption text

This function modifies a caption text by ensuring it ends with a period
and appending an additional caption if provided. Ensure the additional
caption ends also with a period before adding the additional text.

## Usage

``` r
addCaptionTextAddon(captiontext, plotCaptionAddon)
```

## Arguments

- captiontext:

  A character string representing the main caption text.

- plotCaptionAddon:

  A character string representing the additional caption text to be
  added. If NULL or NA, it will not be added.

## Value

A modified caption text with the additional caption added if applicable.
