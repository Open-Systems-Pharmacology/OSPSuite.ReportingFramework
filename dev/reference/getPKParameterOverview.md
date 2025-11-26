# Get PK Parameter Overview

This function retrieves and compiles pharmacokinetic (PK) parameter data
from specified scenario definitions and a PK parameter Excel file. It
processes the data by reading the relevant sheets and merging them with
scenario information.

## Usage

``` r
getPKParameterOverview(projectConfiguration)
```

## Arguments

- projectConfiguration:

  An object of class \`ProjectConfiguration\` containing paths to Excel
  files.

## Value

A \`data.table\` containing the merged PK parameter data along with
associated scenario names.
