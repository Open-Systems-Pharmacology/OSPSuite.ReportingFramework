# Performance: Replace rbind-in-loop with rbindlist for data aggregation

**Priority: High (P1)**
**Estimated Impact: 50-90% speedup for data loading operations**

## Problem

Multiple functions in the codebase use the anti-pattern of growing data structures with `rbind()` inside loops, which causes O(n²) complexity due to repeated memory allocation and copying. This significantly impacts performance when processing large datasets.

## Affected Files and Locations

1. **R/utilities-data.R (lines 50-86)** - `readObservedDataByDictionary()`
   - Pattern: `dataDT <- rbind(dataDT, convertDataByDictionary(...), fill = TRUE)`

2. **R/utilities-timeprofile.R (lines 14-35)** - `loadScenarioTimeProfiles()`
   - Pattern: `dtSimulated <- rbind(dtSimulated, getSimulatedTimeprofile(...), fill = TRUE)`

3. **R/plotDemographics.R (line 785)** - Multiple rbind operations in loops
   - Pattern: `dtPop <- rbind(dtPop, dtPopc, fill = TRUE)`

4. **R/PlotDataTimeProfile.R** - Multiple locations with similar patterns

## Solution

Replace the rbind-in-loop pattern with a two-step approach:

1. **Collect results in a list using `lapply()`**
2. **Combine once at the end using `data.table::rbindlist()`**

### Example Refactoring

**Before:**
```r
dataDT <- data.table()
for (d in split(dataList, seq_len(nrow(dataList)))) {
  dataDT <- rbind(dataDT, convertDataByDictionary(...), fill = TRUE)
}
```

**After:**
```r
dataList_processed <- lapply(split(dataList, seq_len(nrow(dataList))), function(d) {
  convertDataByDictionary(...)
})
dataDT <- rbindlist(dataList_processed, fill = TRUE)
```

## Expected Impact

- **Performance improvement**: 50-90% speedup for data loading operations
- **Memory efficiency**: Reduced memory allocations and garbage collection
- **Scalability**: Better performance with larger datasets

## Implementation Guidelines

1. For each affected function:
   - Initialize an empty list instead of data.table
   - Use `lapply()` or loop to collect results in list
   - Call `data.table::rbindlist(list, fill = TRUE)` once at the end

2. Preserve existing behavior:
   - Keep the `fill = TRUE` parameter for handling different column sets
   - Maintain error handling and validation logic
   - Ensure column ordering and types remain consistent

3. Testing:
   - Verify existing tests still pass
   - Test with varying data sizes (small, medium, large)
   - Confirm output matches original implementation

## References

- data.table documentation: https://rdatatable.gitlab.io/data.table/reference/rbindlist.html
- R best practices: Avoid growing objects in loops

## Labels

`performance`, `enhancement`, `good first issue`
