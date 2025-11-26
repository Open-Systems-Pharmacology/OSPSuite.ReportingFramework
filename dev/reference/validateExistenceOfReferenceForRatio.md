# Validate Existence of Reference for Ratio Plots

This function checks if there are valid reference scenarios for the
plots specified in \`configTablePlots\`. It ensures that at least one
reference scenario is selected for each plot name.

## Usage

``` r
validateExistenceOfReferenceForRatio(configTablePlots, pkParameterDT)
```

## Arguments

- configTablePlots:

  A data.table containing configuration for plots, including
  referenceScenario and plotName columns.

- pkParameterDT:

  A data.table containing pharmacokinetic parameters including scenario
  names and population IDs (not used in this function, but may be
  relevant for context).

## Value

None. The function will stop execution if validation fails, otherwise
returns invisibly.
