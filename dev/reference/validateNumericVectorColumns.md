# validate Numeric Range Columns

Validates numeric range columns in the provided data frame.

## Usage

``` r
validateNumericVectorColumns(columns, data, ...)
```

## Arguments

- columns:

  A vector of column names to validate.

- data:

  A data frame containing the columns to validate.

- ...:

  additionally parameters parsed to checkmate::assertNumeric

## Value

invisible(NULL). Throws an error if validation fails.
