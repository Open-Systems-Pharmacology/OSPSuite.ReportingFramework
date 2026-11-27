# Merge multiple Qmd files into one.

Merge multiple Qmd files into one.

## Usage

``` r
mergeQmds(
  newName = "appendix",
  title = "Appendix",
  sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures"),
  projectConfiguration
)
```

## Arguments

- newName:

  Output file name (default "appendix").

- title:

  Output title (default "Appendix").

- sourceRmds:

  Source Qmd files to include.

- projectConfiguration:

  ProjectConfiguration object.

## Value

Invisible \`NULL\`.
