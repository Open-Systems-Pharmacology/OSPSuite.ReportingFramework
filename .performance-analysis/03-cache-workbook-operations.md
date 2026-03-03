# Performance: Cache and batch Excel workbook operations

**Priority: Medium (P3)**
**Estimated Impact: 20-40% reduction in I/O time**

## Problem

Multiple functions in the codebase repeatedly load and save the same Excel workbooks, causing unnecessary I/O overhead. Each `openxlsx::loadWorkbook()` and `saveWorkbook()` operation involves disk I/O and parsing, which is expensive when done repeatedly on the same file.

## Affected Files and Locations

1. **R/utilities-data.R (lines 99-112)** - Sequential workbook operations
   ```r
   # These functions each load the same plotsFile independently:
   updateDataGroupId(projectConfiguration)      # Loads plotsFile
   updateOutputPathId(projectConfiguration)     # Loads plotsFile again
   updateSimulationId(projectConfiguration)     # Loads plotsFile again
   ```

2. **R/utilities-data.R (lines 505-543)** - `updateDataGroupId()`
   ```r
   wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
   dtDataGroupIds <- xlsxReadData(wb = wb, sheetName = "DataGroups")
   # ... processing ...
   xlsxWriteData(wb = wb, sheetName = "DataGroups", dt = dtDataGroupIds)
   openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
   ```

3. **R/utilities-data.R (lines 563-586)** - `updateOutputPathId()`
   - Immediately loads the same workbook that was just saved

4. **R/utilities-populations.R (lines 530-558)** - Workbook loaded in serial then processed
   ```r
   wb <- openxlsx::loadWorkbook(paramsXLSpath)
   sheets <- intersect(openxlsx::sheets(wb), sheets)
   # ... then parallel processing re-reads from same file
   ```

## Solution

Implement a workbook caching strategy with three approaches:

### Approach 1: Load Once, Pass Around
Load workbook once and pass it to multiple functions:

**Before:**
```r
updateDataGroupId(projectConfiguration)
updateOutputPathId(projectConfiguration)
updateSimulationId(projectConfiguration)
```

**After:**
```r
wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
wb <- updateDataGroupId(projectConfiguration, wb)
wb <- updateOutputPathId(projectConfiguration, wb)
wb <- updateSimulationId(projectConfiguration, wb)
openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
```

### Approach 2: Batch Operations Function
Create a higher-level function that handles all updates:

```r
updateAllWorkbookData <- function(projectConfiguration) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # Update all sheets
  wb <- updateDataGroupIdInternal(wb, projectConfiguration)
  wb <- updateOutputPathIdInternal(wb, projectConfiguration)
  wb <- updateSimulationIdInternal(wb, projectConfiguration)

  # Save once
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible(wb))
}
```

### Approach 3: Workbook Cache Manager
For more complex scenarios, implement a simple cache:

```r
.workbookCache <- new.env(parent = emptyenv())

loadWorkbookCached <- function(path) {
  if (!exists(path, envir = .workbookCache)) {
    .workbookCache[[path]] <- openxlsx::loadWorkbook(path)
  }
  return(.workbookCache[[path]])
}

clearWorkbookCache <- function() {
  rm(list = ls(.workbookCache), envir = .workbookCache)
}
```

## Expected Impact

- **Performance improvement**: 20-40% reduction in I/O time for workflows with multiple Excel operations
- **Resource efficiency**: Reduced disk I/O and file system overhead
- **Better user experience**: Faster execution of data update workflows

## Implementation Guidelines

1. **Phase 1 - Refactor individual functions**:
   - Add optional `wb` parameter to update functions
   - Keep backward compatibility (load if not provided)
   - Return modified workbook object

2. **Phase 2 - Update calling code**:
   - Identify code paths that call multiple update functions
   - Refactor to use single load/save cycle
   - Add tests to verify correctness

3. **Phase 3 - Add batch operations** (optional):
   - Create convenience functions for common workflows
   - Document the new patterns in package vignettes

4. **Backward compatibility**:
   ```r
   updateDataGroupId <- function(projectConfiguration, wb = NULL) {
     loadedHere <- is.null(wb)
     if (loadedHere) {
       wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
     }

     # ... do work ...

     if (loadedHere) {
       openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
     }

     return(invisible(wb))
   }
   ```

## Testing Strategy

1. **Unit tests**:
   - Verify functions work with pre-loaded workbook
   - Verify functions work without workbook (backward compatibility)
   - Test error handling when workbook is invalid

2. **Integration tests**:
   - Test full workflow with batched operations
   - Verify file contents are identical to sequential approach
   - Test concurrent access scenarios

3. **Performance benchmarks**:
   ```r
   # Benchmark before/after
   bench::mark(
     before = {
       updateDataGroupId(config)
       updateOutputPathId(config)
       updateSimulationId(config)
     },
     after = {
       updateAllWorkbookData(config)
     }
   )
   ```

## Risks and Considerations

1. **Concurrent access**: Ensure workbook cache is invalidated appropriately
2. **Memory usage**: Large workbooks kept in memory longer
3. **Error handling**: If one operation fails, workbook may be in inconsistent state

## References

- openxlsx documentation: https://ycphs.github.io/openxlsx/
- R object caching patterns: https://r-pkgs.org/data.html#sec-data-state

## Labels

`performance`, `enhancement`, `io-operations`
