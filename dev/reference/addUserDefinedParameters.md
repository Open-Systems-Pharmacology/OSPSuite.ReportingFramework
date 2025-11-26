# Add User-Defined PK Parameters to OSP-Suite

This function adds user-defined pharmacokinetic (PK) parameters to
OSP-Suite based on the provided definitions. It checks for uniqueness
and existence of parameters before adding them.

## Usage

``` r
addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter)
```

## Arguments

- userdefinedParameters:

  A vector of user-defined PK parameter names to be added.

- dtUserdefPKParameter:

  A data frame containing definitions for user-defined PK parameters.

## Value

NULL. The function updates the OSPSuite environment and does not return
a value. throws Error if user-defined parameters are not defined or are
not unique.
