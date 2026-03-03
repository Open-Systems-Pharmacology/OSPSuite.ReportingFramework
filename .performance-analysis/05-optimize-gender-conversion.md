# Performance: Optimize gender conversion with data.table fcase

**Priority: Low (P5)**
**Estimated Impact: 5-15% speedup for demographic data processing**

## Problem

The gender conversion code in `convertBiometrics()` uses multiple sequential `ifelse()` operations, each of which creates intermediate vectors and scans the entire column. This is inefficient compared to data.table's `fcase()` function which evaluates conditions once.

## Affected Files and Locations

**R/utilities-data.R (lines 470-477)**

```r
data[, gender := ifelse(gender == 1, "MALE", gender)]
data[, gender := ifelse(gender == 2, "FEMALE", gender)]
data[, gender := ifelse(gender == "M", "MALE", gender)]
data[, gender := ifelse(gender == "F", "FEMALE", gender)]
data[, gender := toupper(gender)]
data[, gender := ifelse(gender != "MALE" & gender != "FEMALE", "UNKNOWN", gender)]
```

### Issues

1. **Multiple passes**: Each `ifelse()` scans the entire column
2. **Intermediate vectors**: Each operation creates temporary vectors
3. **Redundant conversions**: Values converted multiple times unnecessarily
4. **Order dependency**: The sequence of operations matters, making it fragile

## Solution

Use data.table's `fcase()` function for efficient multi-condition evaluation:

### Refactored Code

**Before:**
```r
data[, gender := ifelse(gender == 1, "MALE", gender)]
data[, gender := ifelse(gender == 2, "FEMALE", gender)]
data[, gender := ifelse(gender == "M", "MALE", gender)]
data[, gender := ifelse(gender == "F", "FEMALE", gender)]
data[, gender := toupper(gender)]
data[, gender := ifelse(gender != "MALE" & gender != "FEMALE", "UNKNOWN", gender)]
```

**After:**
```r
data[, gender := fcase(
  gender %in% c(1, "1", "M", "m", "male", "Male", "MALE"), "MALE",
  gender %in% c(2, "2", "F", "f", "female", "Female", "FEMALE"), "FEMALE",
  default = "UNKNOWN"
)]
```

### Alternative with explicit handling

If you want to be more explicit about each conversion:

```r
data[, gender := fcase(
  gender == 1 | gender == "1", "MALE",
  gender == 2 | gender == "2", "FEMALE",
  toupper(as.character(gender)) == "M", "MALE",
  toupper(as.character(gender)) == "F", "FEMALE",
  toupper(as.character(gender)) == "MALE", "MALE",
  toupper(as.character(gender)) == "FEMALE", "FEMALE",
  default = "UNKNOWN"
)]
```

### Even more robust version

Handle mixed types and case-insensitivity comprehensively:

```r
data[, gender := {
  # Normalize to character and uppercase
  g <- toupper(as.character(gender))
  # Apply mapping
  fcase(
    g %in% c("1", "M", "MALE"), "MALE",
    g %in% c("2", "F", "FEMALE"), "FEMALE",
    default = "UNKNOWN"
  )
}]
```

## Expected Impact

- **Performance improvement**: 5-15% speedup for demographic data processing
- **Code clarity**: Single, clear statement of intent
- **Maintainability**: Easier to understand and modify
- **Robustness**: Single-pass evaluation reduces chance of errors

## Benchmarking

```r
library(data.table)
library(bench)

# Generate test data
N <- 1e6
test_data <- data.table(
  gender = sample(c(1, 2, "M", "F", "m", "f", "Male", "Female", "unknown", NA), N, replace = TRUE)
)

# Benchmark
bench::mark(
  old = {
    dt <- copy(test_data)
    dt[, gender := ifelse(gender == 1, "MALE", gender)]
    dt[, gender := ifelse(gender == 2, "FEMALE", gender)]
    dt[, gender := ifelse(gender == "M", "MALE", gender)]
    dt[, gender := ifelse(gender == "F", "FEMALE", gender)]
    dt[, gender := toupper(gender)]
    dt[, gender := ifelse(gender != "MALE" & gender != "FEMALE", "UNKNOWN", gender)]
  },
  new = {
    dt <- copy(test_data)
    dt[, gender := {
      g <- toupper(as.character(gender))
      fcase(
        g %in% c("1", "M", "MALE"), "MALE",
        g %in% c("2", "F", "FEMALE"), "FEMALE",
        default = "UNKNOWN"
      )
    }]
  },
  check = FALSE,
  iterations = 10
)
```

## Implementation Guidelines

1. **Preserve behavior**:
   - Test with all known input formats
   - Ensure UNKNOWN is assigned for invalid values
   - Handle NA values appropriately

2. **Add test cases**:
   ```r
   test_that("gender conversion handles all formats", {
     dt <- data.table(gender = c(1, 2, "M", "F", "m", "f", "Male", "Female", "unknown", NA))
     # Apply conversion
     # Verify results
     expect_equal(dt$gender[1:2], c("MALE", "FEMALE"))
     expect_equal(dt$gender[3:4], c("MALE", "FEMALE"))
     # etc.
   })
   ```

3. **Documentation**:
   - Document expected input formats
   - Explain UNKNOWN category
   - Add examples to function documentation

## Additional Considerations

### Handle NA values explicitly

```r
data[, gender := {
  g <- toupper(as.character(gender))
  fcase(
    is.na(g) | g == "NA", "UNKNOWN",
    g %in% c("1", "M", "MALE"), "MALE",
    g %in% c("2", "F", "FEMALE"), "FEMALE",
    default = "UNKNOWN"
  )
}]
```

### Create a reusable function

```r
#' Normalize gender values to standard format
#'
#' @param gender Character or numeric vector of gender values
#' @return Character vector with values "MALE", "FEMALE", or "UNKNOWN"
#' @examples
#' normalizeGender(c(1, 2, "M", "F", "unknown"))
normalizeGender <- function(gender) {
  g <- toupper(as.character(gender))
  data.table::fcase(
    g %in% c("1", "M", "MALE"), "MALE",
    g %in% c("2", "F", "FEMALE"), "FEMALE",
    default = "UNKNOWN"
  )
}

# Usage in convertBiometrics
data[, gender := normalizeGender(gender)]
```

## Testing Strategy

1. **Unit tests** for all input formats:
   - Numeric: 1, 2, 0, 3, etc.
   - Character: "M", "F", "m", "f", "Male", "Female", "MALE", "FEMALE"
   - Invalid: "unknown", "other", "", NA

2. **Integration tests**:
   - Test within full `convertBiometrics()` workflow
   - Verify compatibility with rest of biometrics processing

3. **Performance tests**:
   - Benchmark with various data sizes
   - Compare memory usage

## References

- data.table fcase: https://rdatatable.gitlab.io/data.table/reference/fcase.html
- Efficient data.table operations: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

## Labels

`performance`, `enhancement`, `code-quality`, `good first issue`
