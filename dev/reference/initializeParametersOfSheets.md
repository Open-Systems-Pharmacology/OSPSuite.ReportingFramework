# Initialize PK Parameters in Specified Sheets

This function initializes pharmacokinetic (PK) parameters in the
specified sheets based on user-defined settings. It updates the
parameters in-place within the provided PK parameter configuration
sheet.

## Usage

``` r
initializeParametersOfSheets(projectConfiguration, pkParameterSheets)
```

## Arguments

- projectConfiguration:

  A list containing project configuration settings, including the path
  to the PK parameter configuration sheet

- pkParameterSheets:

  A vector of sheet names that contain the PK parameters to be
  initialized.

## Value

NULL. The function updates parameters in-place and does not return a
value.
