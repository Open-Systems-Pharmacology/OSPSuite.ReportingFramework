# Add Time Range Tags to Data

This function adds time range tags to the provided data based on the
configuration table.

## Usage

``` r
addTimeRangeTagsToData(
  timeRangeColumns,
  dataOld,
  configTable,
  applicationTimes,
  timeTags
)
```

## Arguments

- timeRangeColumns:

  A vector of time range column names.

- dataOld:

  A data table containing the old data.

- configTable:

  A data frame containing the configuration data.

- applicationTimes:

  A list of application times.

## Value

A data table with added time range tags.
