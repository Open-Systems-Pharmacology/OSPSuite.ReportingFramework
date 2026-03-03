# Performance: Vectorize unit conversion operations in data.table

**Priority: High (P2)**
**Estimated Impact: 30-50% speedup for PK parameter processing**

## Problem

Multiple functions use row-by-row iteration or `apply()` for unit conversion operations, which is significantly slower than vectorized data.table operations. The `apply()` function on data.table rows is particularly inefficient as it converts data to matrix and processes row-by-row.

## Affected Files and Locations

1. **R/utilities-pkParameter.R (lines 396-405)** - Unit conversion loop
   ```r
   for (iRow in seq_len(nrow(dtPkParameterDefinition))) {
     dtPkParameterDefinition$unitFactor[iRow] <-
       ospsuite::toUnit(
         quantityOrDimension = ospsuite::getDimensionForUnit(dtPkParameterDefinition$unit[iRow]),
         values = 1,
         sourceUnit = dtPkParameterDefinition$unit[iRow],
         targetUnit = dtPkParameterDefinition$displayUnit[iRow],
         molWeight = as.double(dtPkParameterDefinition$molweight[iRow])
       )
   }
   ```

2. **R/utilities-timeprofile.R (lines 62-71)** - Apply with row-wise operation
   ```r
   dtUnit[, unitFactor := apply(dtUnit, 1, function(row) {
     ospsuite::toUnit(
       quantityOrDimension = row["dimension"],
       values = 1,
       sourceUnit = row["yUnit"],
       targetUnit = row["displayUnit"],
       molWeight = as.numeric(row["molWeight"]),
       molWeightUnit = "g/mol"
     )
   })]
   ```

3. **R/PlotDataTimeProfile.R (line 982)** - Similar patterns with apply

## Solution

Replace row-wise operations with vectorized data.table operations using the `by = .I` pattern or direct vectorization where possible.

### Example Refactoring

**Before:**
```r
for (iRow in seq_len(nrow(dtPkParameterDefinition))) {
  dtPkParameterDefinition$unitFactor[iRow] <-
    ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(dtPkParameterDefinition$unit[iRow]),
      values = 1,
      sourceUnit = dtPkParameterDefinition$unit[iRow],
      targetUnit = dtPkParameterDefinition$displayUnit[iRow],
      molWeight = as.double(dtPkParameterDefinition$molweight[iRow])
    )
}
```

**After:**
```r
dtPkParameterDefinition[, unitFactor := ospsuite::toUnit(
  quantityOrDimension = ospsuite::getDimensionForUnit(unit),
  values = 1,
  sourceUnit = unit,
  targetUnit = displayUnit,
  molWeight = as.double(molweight)
), by = .I]
```

**Alternative (if function can be fully vectorized):**
```r
dtPkParameterDefinition[, unitFactor := {
  dims <- ospsuite::getDimensionForUnit(unit)
  ospsuite::toUnit(
    quantityOrDimension = dims,
    values = rep(1, .N),
    sourceUnit = unit,
    targetUnit = displayUnit,
    molWeight = as.double(molweight)
  )
}]
```

## Expected Impact

- **Performance improvement**: 30-50% speedup for PK parameter and time profile processing
- **Code simplification**: More idiomatic data.table code
- **Better scalability**: Performance improvement increases with data size

## Implementation Guidelines

1. For each affected function:
   - Check if `ospsuite::toUnit()` and similar functions can handle vectorized inputs
   - If yes, use direct vectorization without `by = .I`
   - If no, use `by = .I` for row-wise operation (still faster than for-loop)
   - Remove the `apply()` calls entirely

2. Testing considerations:
   - Verify numerical precision is maintained (unit conversions are exact)
   - Test edge cases (NA values, zero values, extreme values)
   - Ensure column types remain consistent
   - Validate against existing test suite

3. Documentation:
   - Add inline comments explaining the vectorization strategy
   - Document any assumptions about input data structure

## Alternative Solutions

If `ospsuite::toUnit()` cannot be vectorized:
- Consider adding a vectorized wrapper function
- Use `mapply()` or `purrr::pmap()` as intermediate solution
- Submit enhancement request to ospsuite package for vectorized API

## References

- data.table documentation: https://rdatatable.gitlab.io/data.table/
- Efficient row-wise operations: https://stackoverflow.com/questions/3505701/grouping-functions-tapply-by-aggregate-and-the-apply-family

## Labels

`performance`, `enhancement`, `data-processing`
