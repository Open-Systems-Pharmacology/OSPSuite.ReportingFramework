# Convert and Shift Time Units

This function converts time values to a target unit and applies an
optional time offset.

## Usage

``` r
convertAndShiftTimeUnits(timeprofile, targetTimeUnit, timeOffset = 0)
```

## Arguments

- timeprofile:

  A data.table containing time profile data.

- targetTimeUnit:

  The target time unit for conversion.

- timeOffset:

  An optional time offset to be applied after conversion (default is 0).

## Value

A data.table with the time values converted and shifted.
