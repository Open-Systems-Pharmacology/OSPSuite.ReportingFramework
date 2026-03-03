# Performance: Optimize data.table column operations

**Priority: Low (P6)**
**Estimated Impact: 5-10% speedup for data transformation operations**

## Problem

Several functions use inefficient data.table operations, including:
1. Row-by-row column assignments instead of vectorized operations
2. Inefficient use of `.SD` and `lapply` for simple checks
3. Non-idiomatic data.table syntax that misses optimization opportunities

## Affected Files and Locations

### 1. R/utilities-data.R (lines 425-427) - Row-by-row column assignment

```r
for (iRow in seq_len(nrow(dictColumns))) {
  data[, (dictColumns$targetColumn[iRow]) := data[[dictColumns$sourceColumn[iRow]]]]
}
```

**Issue**: Looping over rows to assign columns one at a time.

**Solution**: Use vectorized column assignment:

```r
# Option 1: Use Map/mapply
data[, (dictColumns$targetColumn) := Map(function(src) data[[src]], dictColumns$sourceColumn)]

# Option 2: Direct vectorized assignment (if columns exist)
for (i in seq_len(nrow(dictColumns))) {
  set(data, j = dictColumns$targetColumn[i], value = data[[dictColumns$sourceColumn[i]]])
}

# Option 3: Most efficient - batch operation
sourceValues <- lapply(dictColumns$sourceColumn, function(col) data[[col]])
data[, (dictColumns$targetColumn) := sourceValues]
```

### 2. R/utilities-plot.R (lines 763-779) - Inefficient NA check

```r
configTablePlots <- configTable[is.na(level)]
if (!all(configTablePlots[,
  lapply(.SD, function(x) {
    all(is.na(x))
  }),
  .SDcols = "header"
])) {
  stop("Invalid plot configuration table. Missing header for level")
}
```

**Issue**: Using `lapply(.SD)` for a simple column check is overkill.

**Solution**:

```r
configTablePlots <- configTable[is.na(level)]
if (!all(is.na(configTablePlots$header))) {
  stop("Invalid plot configuration table. Missing header for level")
}
```

### 3. R/utilities-data.R (line 242) - Nested paste with apply

```r
paste(apply(tmp[, !"nUnit", with = FALSE], 1, function(x) {
  paste(x, collapse = " ")
}), collapse = " | ")
```

**Issue**: Using `apply` with nested `paste` operations is very slow.

**Solution**:

```r
# More efficient: use data.table's built-in operations
tmp[, paste(do.call(paste, c(.SD, sep = " ")), collapse = " | "), .SDcols = !c("nUnit")]

# Or even simpler if you just need to concatenate all values:
tmp[, paste(.SD, collapse = " | "), .SDcols = !c("nUnit")]
```

### 4. R/utilities-data.R (lines 459-468) - Loop with DT column updates

```r
for (col in intersect(names(BIOMETRICUNITS), dict$targetColumn)) {
  unitFactor <- ospsuite::toUnit(...)
  data[, (col) := get(col) * unitFactor]
}
```

**Issue**: While not terrible, could potentially vectorize unit conversions.

**Solution**:

```r
# If multiple columns need same conversion, batch them
colsToConvert <- intersect(names(BIOMETRICUNITS), dict$targetColumn)
unitFactors <- sapply(colsToConvert, function(col) {
  ospsuite::toUnit(...)
})

# Apply all conversions at once
data[, (colsToConvert) := Map(function(col, factor) get(col) * factor,
                                colsToConvert, unitFactors)]
```

## Expected Impact

- **Performance improvement**: 5-10% speedup for data transformation operations
- **Code quality**: More idiomatic data.table code
- **Maintainability**: Clearer intent, easier to understand

## Solution Examples

### Example 1: Efficient Column Copying

**Before:**
```r
for (iRow in seq_len(nrow(dictColumns))) {
  data[, (dictColumns$targetColumn[iRow]) := data[[dictColumns$sourceColumn[iRow]]]]
}
```

**After:**
```r
# Method 1: Batch assignment (most efficient)
data[, (dictColumns$targetColumn) := lapply(dictColumns$sourceColumn, function(col) .SD[[col]]),
     .SDcols = dictColumns$sourceColumn]

# Method 2: Using set() in loop (still better than :=)
for (i in seq_len(nrow(dictColumns))) {
  set(data,
      j = dictColumns$targetColumn[i],
      value = data[[dictColumns$sourceColumn[i]]])
}

# Method 3: Vectorized with Map
sourceValues <- lapply(dictColumns$sourceColumn, function(col) data[[col]])
data[, (dictColumns$targetColumn) := sourceValues]
```

### Example 2: Efficient String Aggregation

**Before:**
```r
paste(apply(tmp[, !"nUnit", with = FALSE], 1, function(x) {
  paste(x, collapse = " ")
}), collapse = " | ")
```

**After:**
```r
# Use data.table's do.call approach
tmp[, do.call(paste, c(.SD, sep = " ")), .SDcols = !c("nUnit")] %>%
  paste(collapse = " | ")

# Or more explicitly
tmp[, .(row_text = do.call(paste, c(.SD, sep = " "))),
    .SDcols = !c("nUnit")][, paste(row_text, collapse = " | ")]
```

### Example 3: Simple Column Check

**Before:**
```r
if (!all(configTablePlots[,
  lapply(.SD, function(x) {
    all(is.na(x))
  }),
  .SDcols = "header"
])) {
  stop("Invalid plot configuration table. Missing header for level")
}
```

**After:**
```r
if (!all(is.na(configTablePlots$header))) {
  stop("Invalid plot configuration table. Missing header for level")
}
```

## Implementation Guidelines

1. **Identify patterns**:
   - Look for row-wise operations that could be vectorized
   - Find `lapply(.SD)` used for single column operations
   - Locate nested loops that could use data.table operations

2. **Refactor incrementally**:
   - Change one pattern at a time
   - Test after each change
   - Ensure output remains identical

3. **Use data.table idioms**:
   - `:=` for column assignment by reference
   - `set()` for programmatic column updates
   - `.SD` for operating on subsets of columns
   - `.SDcols` for selecting columns

4. **Benchmark changes**:
   ```r
   library(bench)

   bench::mark(
     old = { /* old code */ },
     new = { /* new code */ },
     check = TRUE,  # Verify results are identical
     iterations = 100
   )
   ```

## Testing Strategy

1. **Unit tests**:
   - Test each refactored function independently
   - Verify output matches original implementation
   - Test with edge cases (empty data, single row, large data)

2. **Integration tests**:
   - Run full workflow with refactored code
   - Compare outputs to baseline

3. **Performance tests**:
   - Benchmark with representative data sizes
   - Measure both time and memory

## Common data.table Performance Patterns

### Pattern 1: Vectorized Column Assignment
```r
# Bad
for (col in cols) {
  dt[, (col) := some_function(get(col))]
}

# Good
dt[, (cols) := lapply(.SD, some_function), .SDcols = cols]
```

### Pattern 2: Conditional Updates
```r
# Bad
for (i in 1:nrow(dt)) {
  if (condition) {
    dt[i, col := value]
  }
}

# Good
dt[condition, col := value]
```

### Pattern 3: Group-wise Operations
```r
# Bad
for (grp in unique(dt$group)) {
  dt[group == grp, result := compute(value)]
}

# Good
dt[, result := compute(value), by = group]
```

### Pattern 4: Multiple Column Operations
```r
# Bad
dt[, newcol1 := f1(col1)]
dt[, newcol2 := f2(col2)]
dt[, newcol3 := f3(col3)]

# Good
dt[, `:=`(
  newcol1 = f1(col1),
  newcol2 = f2(col2),
  newcol3 = f3(col3)
)]
```

## References

- data.table best practices: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
- data.table reference semantics: https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html
- Advanced data.table: https://rdatatable.gitlab.io/data.table/articles/datatable-sd-usage.html

## Labels

`performance`, `enhancement`, `code-quality`, `data-table`
