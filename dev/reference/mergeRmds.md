# Creates Rmd file which include specified .Rmd s

Creates Rmd file which include specified .Rmd s

## Usage

``` r
mergeRmds(
  newName = "appendix",
  title = "Appendix",
  sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures"),
  projectConfiguration
)
```

## Arguments

- newName:

  new Name of .Rmd

- title:

  Title of new Rmd

- sourceRmds:

  list of .rmds to include

- projectConfiguration:

  Object of class \`ProjectConfiguration\` containing information on
  paths and file names
