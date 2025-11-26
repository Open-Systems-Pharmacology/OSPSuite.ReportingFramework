# Validate Structure and Content of PK Parameter Data Table

This function validates the structure and content of the provided
pharmacokinetic parameter data table (\`pkParameterDT\`). It checks for
the presence of required columns and ensures that the combination of
\`outputPathId\` and \`parameter\` is unique with consistent
\`displayUnitPKParameter\`.

## Usage

``` r
validatePKParameterDT(pkParameterDT)
```

## Arguments

- pkParameterDT:

  A data.table containing pharmacokinetic parameters with the required
  columns: scenario, pkParameter, individualId, value, outputPathId,
  displayNamePKParameter, and displayUnitPKParameter.

## Value

NULL. The function will stop execution if validation fails, otherwise
returns invisibly.
