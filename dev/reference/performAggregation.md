# Perform Aggregation

This function performs the aggregation of observed data based on the
specified criteria.

## Usage

``` r
performAggregation(dataToAggregate, aggregationFun, aggrCriteria)
```

## Arguments

- dataToAggregate:

  A data.table containing the data to be aggregated. It must include the
  column 'yValues' and optionally 'lloq'.

- aggregationFun:

  A function to aggregate the data. This function should accept a
  numeric vector and return a list with aggregated values.

- aggrCriteria:

  A character vector specifying the columns to group by.

## Value

A data.table containing aggregated results with counts
(\`numberOfIndividuals\`), aggregated values, and the number of
measurements below the lower limit of quantification (\`nBelowLLOQ\`).
