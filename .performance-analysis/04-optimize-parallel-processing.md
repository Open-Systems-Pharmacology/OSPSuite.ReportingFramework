# Performance: Optimize parallel processing overhead

**Priority: Medium (P4)**
**Estimated Impact: 10-30% speedup by avoiding unnecessary overhead**

## Problem

The codebase sets up parallel processing clusters unconditionally, even for small workloads where the overhead of creating/destroying clusters exceeds the performance benefits. Additionally, the current implementation uses socket clusters which have higher overhead than alternatives on Unix systems.

## Affected Files and Locations

1. **R/utilities-populations.R (lines 537-558)** - Unconditional parallelization
   ```r
   # Set up the parallel backend
   numCores <- parallel::detectCores() - 1
   cl <- parallel::makeCluster(numCores)
   doParallel::registerDoParallel(cl)

   # Use foreach to parallelize the loop
   dtSheets <- foreach::foreach(sheet = sheets, .packages = c("esqlabsR", "data.table", "dplyr")) %dopar% {
     tmp <- esqlabsR::readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet) %>%
       data.table::as.data.table()
     # ...
   }

   # Stop the cluster
   parallel::stopCluster(cl)
   ```

### Issues Identified

1. **Setup/teardown overhead**: Creating and destroying clusters has significant overhead
2. **Small task overhead**: Parallelizing small tasks can be slower than sequential execution
3. **Package loading overhead**: Loading packages in each worker (`.packages`) adds overhead
4. **Socket cluster overhead**: Socket clusters (default on Windows and explicitly used here) are slower than forking on Unix
5. **No cost/benefit analysis**: No check whether parallelization is worthwhile

## Solution

Implement conditional parallelization based on workload size and system capabilities:

### Approach 1: Conditional Parallelization

**Before:**
```r
numCores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(numCores)
doParallel::registerDoParallel(cl)

dtSheets <- foreach::foreach(sheet = sheets, .packages = c("esqlabsR", "data.table", "dplyr")) %dopar% {
  # ... work ...
}

parallel::stopCluster(cl)
```

**After:**
```r
# Only parallelize if worthwhile
PARALLEL_THRESHOLD <- 5  # Minimum number of sheets to justify parallel
USE_PARALLEL <- length(sheets) >= PARALLEL_THRESHOLD && parallel::detectCores() > 2

if (USE_PARALLEL) {
  numCores <- min(parallel::detectCores() - 1, length(sheets))

  # Use forking on Unix (much faster), sockets on Windows
  if (.Platform$OS.type == "unix") {
    doParallel::registerDoParallel(cores = numCores)  # Uses forking
    dtSheets <- foreach::foreach(sheet = sheets, .packages = c("esqlabsR", "data.table", "dplyr")) %dopar% {
      # ... work ...
    }
  } else {
    cl <- parallel::makeCluster(numCores)
    doParallel::registerDoParallel(cl)
    dtSheets <- foreach::foreach(sheet = sheets, .packages = c("esqlabsR", "data.table", "dplyr")) %dopar% {
      # ... work ...
    }
    parallel::stopCluster(cl)
  }
} else {
  # Sequential execution for small workloads
  dtSheets <- lapply(sheets, function(sheet) {
    tmp <- esqlabsR::readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet) %>%
      data.table::as.data.table()
    # ... work ...
  })
}
```

### Approach 2: Smart Parallel Wrapper

Create a utility function to handle parallel execution intelligently:

```r
#' Execute function in parallel only if beneficial
#'
#' @param items List/vector of items to process
#' @param func Function to apply to each item
#' @param threshold Minimum number of items to justify parallelization
#' @param packages Packages needed in parallel workers
#' @return List of results
smartParallelLapply <- function(items, func, threshold = 5, packages = NULL) {
  n_items <- length(items)
  n_cores <- parallel::detectCores()

  # Decide whether to parallelize
  use_parallel <- n_items >= threshold && n_cores > 2

  if (!use_parallel) {
    # Sequential execution
    return(lapply(items, func))
  }

  # Parallel execution
  n_workers <- min(n_cores - 1, n_items)

  if (.Platform$OS.type == "unix") {
    # Use forking on Unix (faster)
    doParallel::registerDoParallel(cores = n_workers)
    result <- foreach::foreach(
      item = items,
      .packages = packages
    ) %dopar% {
      func(item)
    }
  } else {
    # Use socket cluster on Windows
    cl <- parallel::makeCluster(n_workers)
    on.exit(parallel::stopCluster(cl))
    doParallel::registerDoParallel(cl)
    result <- foreach::foreach(
      item = items,
      .packages = packages
    ) %dopar% {
      func(item)
    }
  }

  return(result)
}

# Usage
dtSheets <- smartParallelLapply(
  sheets,
  function(sheet) {
    esqlabsR::readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet) %>%
      data.table::as.data.table() %>%
      # ... processing ...
  },
  packages = c("esqlabsR", "data.table", "dplyr")
)
```

### Approach 3: Use `future` Package

Modern alternative with better performance and simpler API:

```r
library(future.apply)

# Configure based on workload
if (length(sheets) >= 5) {
  plan(multisession, workers = min(availableCores() - 1, length(sheets)))
} else {
  plan(sequential)
}

dtSheets <- future_lapply(sheets, function(sheet) {
  esqlabsR::readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet) %>%
    data.table::as.data.table()
  # ...
}, future.packages = c("esqlabsR", "data.table", "dplyr"))
```

## Expected Impact

- **Performance improvement**: 10-30% speedup by avoiding overhead for small workloads
- **Resource efficiency**: Better CPU utilization
- **Adaptive behavior**: Automatically adjusts to workload and system capabilities
- **Cross-platform optimization**: Better performance on Unix systems

## Implementation Guidelines

1. **Determine threshold**:
   - Benchmark to find crossover point where parallel becomes beneficial
   - Consider both number of items and per-item processing time
   - Default threshold of 5-10 items is reasonable starting point

2. **Refactor existing code**:
   - Add conditional logic or use wrapper function
   - Keep existing behavior as fallback
   - Add configuration option for users to control parallelization

3. **Testing**:
   - Test with varying numbers of sheets (1, 5, 10, 50)
   - Test on both Unix and Windows
   - Verify results are identical regardless of execution mode
   - Add performance benchmarks

4. **Documentation**:
   - Document when parallelization is used
   - Explain threshold parameter
   - Provide guidance for users with different workload sizes

## Configuration Options

Consider adding package options for user control:

```r
# In package initialization or options
options(
  ospsuite.reportingframework.parallel.threshold = 5,
  ospsuite.reportingframework.parallel.cores = NULL  # NULL = auto-detect
)
```

## Performance Benchmarking

Include benchmarking code to validate improvements:

```r
bench::mark(
  sequential = lapply(sheets[1:3], processSheet),
  parallel_old = {
    cl <- makeCluster(2)
    on.exit(stopCluster(cl))
    parLapply(cl, sheets[1:3], processSheet)
  },
  parallel_conditional = smartParallelLapply(sheets[1:3], processSheet)
)
```

## Risks and Considerations

1. **Backward compatibility**: Ensure existing code still works
2. **User expectations**: Users may expect parallelization always on
3. **Testing complexity**: Need to test both sequential and parallel paths
4. **Platform differences**: Forking not available on Windows

## Alternative: future Package Migration

Consider migrating entirely to `future` package for:
- Simpler API
- Better performance
- More execution backends (multisession, multicore, cluster, remote)
- Easier testing and debugging

## References

- parallel package: https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf
- future package: https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
- Parallel R best practices: https://github.com/HenrikBengtsson/future

## Labels

`performance`, `enhancement`, `parallel-processing`
