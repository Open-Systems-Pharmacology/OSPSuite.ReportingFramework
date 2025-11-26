# Filters observed data for individual groups which are suited for aggregation or "Virtual Twin population" creation

Filters observed data for individual groups which are suited for
aggregation or "Virtual Twin population" creation

## Usage

``` r
getIndividualDataGroups(dataObserved, groups, minN = 2)
```

## Arguments

- dataObserved:

  A \`data.table\` containing observed data.

- groups:

  A character vector specifying the groups to aggregate. If NULL, all
  available groups are used.

- minN:

  The minimal number needed for a group.

## Value

A vector with suitable group Ids.
