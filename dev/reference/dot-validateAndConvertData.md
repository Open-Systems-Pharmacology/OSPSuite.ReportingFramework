# Validates observed data and converts it to appropriate format.

This function checks the input data for required columns and formats it
for plotting. If predicted values are needed, it calculates them based
on the observed and simulated data.

## Usage

``` r
.validateAndConvertData(plotData, predictedIsNeeded)
```

## Arguments

- plotData:

  Either a data.table with columns 'xValues', 'yValues', 'group' or an
  object of class 'DataCombined'.

- predictedIsNeeded:

  If TRUE, only observed data are returned. If FALSE and the "predicted"
  column does not exist, predicted values are calculated.

## Value

A \`data.table\` with data formatted for plotting.
