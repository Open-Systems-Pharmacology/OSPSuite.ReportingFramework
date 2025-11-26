# Sets columnType attribute according to dictionary

Sets columnType attribute according to dictionary

## Usage

``` r
setDataTypeAttributes(dataDT, dict = NULL)
```

## Arguments

- dataDT:

  A \`data.table\` with observed data.

- dict:

  Named list with columnNames and columnTypes; if NULL, list is produced
  by template saved in package installation.

## Value

A \`data.table\` with attributes.
