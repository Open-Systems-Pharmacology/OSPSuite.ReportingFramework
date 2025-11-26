# Validate Consistency of Values Within Groups

This function checks whether specified columns in a data table have
consistent values within groups defined by one or more grouping columns.
If any column contains more than one unique value within a group, an
error is raised.

## Usage

``` r
validateGroupConsistency(dt, valueColumns, groupingColumns = "plotName")
```

## Arguments

- dt:

  A data.table or data.frame containing the data to be validated.

- valueColumns:

  A character vector of column names to check for consistency.

- groupingColumns:

  A character vector of column names that define the groups. Default is
  'plotName'.

## Value

Returns NULL (invisible) if all checks pass. Raises an error if any
value column contains inconsistent values within a group.
