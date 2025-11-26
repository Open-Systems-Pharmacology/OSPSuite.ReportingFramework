# Read User-Defined Pharmacokinetic (PK) Parameters

This function reads user-defined pharmacokinetic (PK) parameters from an
Excel file and validates their structure and content.

## Usage

``` r
readUserDefinedPKParameters(file)
```

## Arguments

- file:

  The path to the Excel file containing user-defined PK parameters.

## Value

A data frame of validated user-defined PK parameters. throws Error if
validation fails or if required fields are missing.
