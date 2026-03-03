#!/usr/bin/env node

/**
 * Script to create GitHub issues via GraphQL API for performance optimizations
 *
 * This script creates detailed GitHub issues for each identified performance
 * optimization opportunity in the OSPSuite.ReportingFramework R package.
 *
 * Usage:
 *   node create-performance-issues.js <GITHUB_TOKEN>
 *
 * The script requires a GitHub Personal Access Token with 'repo' scope.
 */

const https = require('https');

const GITHUB_GRAPHQL_API = 'https://api.github.com/graphql';
const REPO_OWNER = 'Open-Systems-Pharmacology';
const REPO_NAME = 'OSPSuite.ReportingFramework';

// Issue definitions
const issues = [
  {
    title: 'Performance: Replace rbind-in-loop with rbindlist in utilities-data.R',
    body: `## Problem

The data loading function in \`R/utilities-data.R\` (lines 50-86) uses the \`rbind()\` anti-pattern in a loop, resulting in O(n²) complexity.

### Current Implementation
\`\`\`r
dataDT <- data.table()
for (d in split(dataList, seq_len(nrow(dataList)))) {
  tmpData <- data.table::fread(...)
  tmpdict <- readDataDictionary(...)

  dataDT <- rbind(dataDT,
    convertDataByDictionary(...) %>%
      dplyr::mutate(dataClass = d$dataClass),
    fill = TRUE
  )
  # ... more processing
}
\`\`\`

### Issues
- \`rbind()\` in a loop has O(n²) complexity due to repeated memory allocation and copying
- Each iteration creates a new data.table object
- Severely impacts performance with large datasets or many files
- Common performance anti-pattern in R

## Solution

Collect data in a list and use \`rbindlist()\` once:

\`\`\`r
dataDTList <- list()
dictList <- list()

for (d in split(dataList, seq_len(nrow(dataList)))) {
  tmpData <- data.table::fread(...)
  tmpdict <- readDataDictionary(...)

  dataDTList[[length(dataDTList) + 1]] <-
    convertDataByDictionary(...) %>%
      dplyr::mutate(dataClass = d$dataClass)

  # ... more processing for dict
}

dataDT <- data.table::rbindlist(dataDTList, fill = TRUE)
\`\`\`

## Implementation Tasks

- [ ] Read and understand current implementation in \`R/utilities-data.R\` lines 50-86
- [ ] Refactor to collect results in a list during loop iteration
- [ ] Replace final rbind with rbindlist after loop completes
- [ ] Test with multiple data files to ensure correctness
- [ ] Benchmark performance improvement using microbenchmark
- [ ] Update dictionary collection logic similarly if applicable
- [ ] Verify all existing unit tests pass
- [ ] Document the optimization in code comments

## Expected Impact

**Estimated Performance Improvement:** 70-90% reduction in execution time when processing multiple files

This optimization is critical for workflows that process many data files.

## File Location

\`R/utilities-data.R:50-86\`

## Testing Instructions

\`\`\`r
# Benchmark template
library(microbenchmark)

# Test with realistic data
test_data <- # ... load test datasets

# Compare before and after
microbenchmark(
  original = original_function(test_data),
  optimized = optimized_function(test_data),
  times = 100
)

# Verify correctness
all.equal(original_result, optimized_result)
\`\`\`

## Labels

performance, enhancement`,
    labels: ['performance', 'enhancement']
  },
  {
    title: 'Performance: Replace rbind-in-loop with rbindlist in utilities-timeprofile.R',
    body: `## Problem

The scenario loading function in \`R/utilities-timeprofile.R\` (lines 14-35) uses the \`rbind()\` anti-pattern in a loop, resulting in O(n²) complexity.

### Current Implementation
\`\`\`r
dtSimulated <- data.table()
for (scenarioName in names(outputPathsPerScenario)) {
  # ... processing
  dtSimulated <- rbind(
    dtSimulated,
    getSimulatedTimeprofile(...) %>%
      dplyr::mutate(scenario = scenarioName),
    fill = TRUE
  )
}
\`\`\`

### Issues
- \`rbind()\` in a loop has O(n²) complexity
- Repeated memory allocation and copying
- Performance degrades significantly with many scenarios
- Same anti-pattern as utilities-data.R

## Solution

Collect results in a list and use \`rbindlist()\` once:

\`\`\`r
dtSimulatedList <- vector("list", length(outputPathsPerScenario))

for (i in seq_along(outputPathsPerScenario)) {
  scenarioName <- names(outputPathsPerScenario)[i]
  # ... processing
  dtSimulatedList[[i]] <- getSimulatedTimeprofile(...) %>%
    dplyr::mutate(scenario = scenarioName)
}

dtSimulated <- data.table::rbindlist(dtSimulatedList, fill = TRUE)
\`\`\`

## Implementation Tasks

- [ ] Read and understand current implementation in \`R/utilities-timeprofile.R\` lines 14-35
- [ ] Pre-allocate list with correct size based on number of scenarios
- [ ] Modify loop to collect results in list instead of rbind
- [ ] Replace final rbind with rbindlist after loop completes
- [ ] Test with multiple scenarios to ensure correctness
- [ ] Benchmark performance improvement using microbenchmark
- [ ] Verify all existing unit tests pass
- [ ] Document the optimization in code comments

## Expected Impact

**Estimated Performance Improvement:** 70-90% reduction in execution time when processing multiple scenarios

This optimization is especially important for workflows with many simulation scenarios.

## File Location

\`R/utilities-timeprofile.R:14-35\`

## Testing Instructions

\`\`\`r
# Test with various numbers of scenarios
test_scenarios <- list(
  single = list(scenario1 = data),
  few = list(scenario1 = data, scenario2 = data, scenario3 = data),
  many = # ... 20+ scenarios
)

# Benchmark and verify correctness for each
lapply(test_scenarios, function(scenarios) {
  result_original <- original_function(scenarios)
  result_optimized <- optimized_function(scenarios)
  all.equal(result_original, result_optimized)
})
\`\`\`

## Labels

performance, enhancement`,
    labels: ['performance', 'enhancement']
  },
  {
    title: 'Performance: Vectorize unit conversion operations in utilities-pkParameter.R',
    body: `## Problem

The unit conversion operation in \`R/utilities-pkParameter.R\` (lines 396-405) uses inefficient row-by-row iteration instead of vectorized operations.

### Current Implementation
\`\`\`r
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
\`\`\`

### Issues
- Row-by-row iteration is inefficient in R
- O(n) complexity with high constant factor
- Not utilizing data.table's vectorization capabilities
- Creates unnecessary intermediate objects

## Solution

Replace with vectorized data.table operation using the \`by = .I\` pattern:

\`\`\`r
dtPkParameterDefinition[, unitFactor := ospsuite::toUnit(
  quantityOrDimension = ospsuite::getDimensionForUnit(unit),
  values = 1,
  sourceUnit = unit,
  targetUnit = displayUnit,
  molWeight = as.double(molweight)
), by = .I]
\`\`\`

## Implementation Tasks

- [ ] Read and understand current implementation in \`R/utilities-pkParameter.R\` lines 396-405
- [ ] Replace for-loop with data.table vectorized operation using \`by = .I\`
- [ ] Test with existing unit test suite to ensure correctness
- [ ] Verify that ospsuite::toUnit works correctly with data.table syntax
- [ ] Benchmark performance improvement using microbenchmark
- [ ] Verify numerical accuracy is maintained (use all.equal)
- [ ] Test with edge cases (empty data, single row, many rows)
- [ ] Document the optimization in code comments

## Expected Impact

**Estimated Performance Improvement:** 50-80% reduction in execution time for this operation

## File Location

\`R/utilities-pkParameter.R:396-405\`

## Testing Instructions

\`\`\`r
# Create test data with various scenarios
test_data <- data.table(
  unit = c("mg", "kg", "L"),
  displayUnit = c("g", "mg", "mL"),
  molweight = c(100, 200, 300)
)

# Benchmark
microbenchmark(
  original = for_loop_version(test_data),
  optimized = vectorized_version(test_data),
  times = 1000
)

# Verify numerical accuracy
all.equal(for_loop_result$unitFactor, vectorized_result$unitFactor)
\`\`\`

## Labels

performance, enhancement, good-first-issue`,
    labels: ['performance', 'enhancement', 'good-first-issue']
  },
  {
    title: 'Performance: Vectorize unit conversion operations in utilities-timeprofile.R',
    body: `## Problem

The unit conversion operation in \`R/utilities-timeprofile.R\` (lines 62-71) uses \`apply()\` which is inefficient compared to vectorized operations.

### Current Implementation
\`\`\`r
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
\`\`\`

### Issues
- \`apply()\` creates implicit copies and is slow
- Row-by-row operation instead of vectorized
- Similar anti-pattern to other unit conversion code
- Unnecessary conversions to/from matrix format

## Solution

Replace with vectorized data.table operation using the \`by = .I\` pattern:

\`\`\`r
dtUnit[, unitFactor := ospsuite::toUnit(
  quantityOrDimension = dimension,
  values = 1,
  sourceUnit = yUnit,
  targetUnit = displayUnit,
  molWeight = as.numeric(molWeight),
  molWeightUnit = "g/mol"
), by = .I]
\`\`\`

## Implementation Tasks

- [ ] Read and understand current implementation in \`R/utilities-timeprofile.R\` lines 62-71
- [ ] Replace apply() with data.table vectorized operation using \`by = .I\`
- [ ] Test with existing unit test suite to ensure correctness
- [ ] Verify that ospsuite::toUnit works correctly with data.table syntax
- [ ] Benchmark performance improvement using microbenchmark
- [ ] Verify numerical accuracy is maintained (use all.equal)
- [ ] Check for any side effects from removing apply()
- [ ] Test with edge cases (empty data, single row, many rows)
- [ ] Document the optimization in code comments

## Expected Impact

**Estimated Performance Improvement:** 50-80% reduction in execution time for this operation

## File Location

\`R/utilities-timeprofile.R:62-71\`

## Testing Instructions

\`\`\`r
# Create test data
test_data <- data.table(
  dimension = c("Length", "Mass", "Time"),
  yUnit = c("m", "kg", "s"),
  displayUnit = c("cm", "g", "min"),
  molWeight = c(100, 200, 300)
)

# Benchmark
microbenchmark(
  apply_version = apply_version(copy(test_data)),
  vectorized_version = vectorized_version(copy(test_data)),
  times = 1000
)

# Verify correctness
all.equal(apply_result$unitFactor, vectorized_result$unitFactor, tolerance = 1e-10)
\`\`\`

## Labels

performance, enhancement, good-first-issue`,
    labels: ['performance', 'enhancement', 'good-first-issue']
  },
  {
    title: 'Performance: Replace rbind with rbindlist in plotDemographics.R',
    body: `## Problem

The demographic plotting function in \`R/plotDemographics.R\` (line 785) uses \`rbind()\` instead of the more efficient \`rbindlist()\`.

### Current Implementation
\`\`\`r
dtPop <- rbind(dtPop, dtPopc, fill = TRUE)
\`\`\`

### Issues
- \`rbind()\` is less efficient than \`rbindlist()\`
- Part of a broader pattern that should be optimized for consistency
- Small but measurable performance impact
- Not following data.table best practices

## Solution

Use \`data.table::rbindlist()\`:

\`\`\`r
dtPop <- data.table::rbindlist(list(dtPop, dtPopc), fill = TRUE)
\`\`\`

## Implementation Tasks

- [ ] Read and understand current implementation in \`R/plotDemographics.R\` line 785
- [ ] Replace rbind with rbindlist
- [ ] Test demographic plotting functionality to ensure correctness
- [ ] Verify output is identical to previous implementation
- [ ] Benchmark performance improvement using microbenchmark
- [ ] Verify all existing unit tests pass
- [ ] Document the optimization in code comments

## Expected Impact

**Estimated Performance Improvement:** 5-10% reduction in execution time

While the improvement is modest, this change promotes best practices and consistency across the codebase.

## File Location

\`R/plotDemographics.R:785\`

## Testing Instructions

\`\`\`r
# Create test demographic data
test_dtPop <- # ... create test data
test_dtPopc <- # ... create test data

# Benchmark
microbenchmark(
  rbind_version = rbind(test_dtPop, test_dtPopc, fill = TRUE),
  rbindlist_version = rbindlist(list(test_dtPop, test_dtPopc), fill = TRUE),
  times = 1000
)

# Verify identical results
all.equal(rbind_result, rbindlist_result)
\`\`\`

## Labels

performance, enhancement, good-first-issue`,
    labels: ['performance', 'enhancement', 'good-first-issue']
  },
  {
    title: 'Performance: Optimize merge operations with data.table native syntax',
    body: `## Problem

Multiple files use base R \`merge()\` operations that could be optimized using data.table's native join syntax and proper indexing. 15+ instances identified.

### Affected Files
- \`R/utilities-xlsx.R\` (line 589)
- \`R/plotDemographics.R\` (lines 264, 526, 593, 613, 652, 694)
- \`R/plotSensitivity.R\` (lines 127, 131, 172, 193, 231)
- \`R/utilities-pkParameter.R\` (lines 210, 264, 359)

### Example Issues

\`\`\`r
# plotSensitivity.R:127
merge(
  configEnv$outputPaths[, c("outputPathId", "displayNameOutput")],
  by = "outputPathId"
)

# utilities-pkParameter.R:210
pkParameterDT <- merge(data.table::rbindlist(pkAnalysesList), ...)
\`\`\`

### Problems
- Base R \`merge()\` syntax doesn't use data.table optimizations
- Missing key setup for efficient joins
- No indexing on join columns
- Slower than data.table's native join operations

## Solution

Optimize merge operations using:

1. **Set keys** before joining: \`setkey(dt, key_column)\`
2. **Use data.table join syntax**: \`dt1[dt2, on = .(col1 = col2)]\`
3. **Create indices** for frequently joined columns: \`setindex(dt, col)\`
4. **Use merge.data.table()** explicitly when base merge syntax is clearer

### Example Optimizations

\`\`\`r
# Before
result <- merge(dt1, dt2, by = "id")

# After - Option 1: Set keys
setkey(dt1, id)
setkey(dt2, id)
result <- dt1[dt2]

# After - Option 2: Use on parameter
result <- dt1[dt2, on = .(id)]

# After - Option 3: Use merge.data.table explicitly
result <- merge.data.table(dt1, dt2, by = "id")
\`\`\`

## Implementation Tasks

- [ ] Identify and catalog all 15+ merge instances across affected files
- [ ] Analyze each merge to determine optimal optimization strategy
- [ ] Prioritize high-impact merges (large data, frequent execution)
- [ ] Add key setup for frequently joined tables where beneficial
- [ ] Convert to data.table join syntax where it improves readability and performance
- [ ] Benchmark performance improvements for critical merges
- [ ] Test all modified merges to ensure correctness
- [ ] Document join strategy for complex merges in code comments
- [ ] Verify all existing unit tests pass

## Expected Impact

**Estimated Performance Improvement:** 20-50% faster merge operations

Impact is most significant with:
- Large datasets (>10,000 rows)
- Multiple merges on same data
- Unindexed join columns

## File Locations

Multiple files - see affected files list above

## Testing Instructions

\`\`\`r
# For each optimized merge:
# 1. Create representative test data
test_dt1 <- data.table(id = 1:10000, value1 = rnorm(10000))
test_dt2 <- data.table(id = 1:10000, value2 = rnorm(10000))

# 2. Benchmark
microbenchmark(
  base_merge = merge(test_dt1, test_dt2, by = "id"),
  dt_on = test_dt1[test_dt2, on = .(id)],
  dt_keyed = {setkey(copy(test_dt1), id); setkey(copy(test_dt2), id); test_dt1[test_dt2]},
  times = 100
)

# 3. Verify correctness
all.equal(base_result, optimized_result, check.attributes = FALSE)
\`\`\`

## Labels

performance, enhancement`,
    labels: ['performance', 'enhancement']
  },
  {
    title: 'Performance: Audit and reduce unnecessary data.table copy() operations',
    body: `## Problem

Multiple files contain \`data.table::copy()\` operations that may be unnecessary, causing memory overhead and performance impact. 14+ instances identified.

### Affected Files
- \`R/utilities-xlsx.R\` (lines 66, 502)
- \`R/plotDemographics.R\` (line 219)
- \`R/utilities-pkParameter.R\` (line 439)
- \`R/plotPKForest.R\` (line 584)
- \`R/plotTimeProfilePanels.R\` (line 1211)
- \`R/utilities-data.R\` (line 694)
- \`R/ospsuite_plotTimeprofile.R\` (lines 437, 517)
- \`R/utilities-configurationEnvironment.R\` (line 170)

### Example Issues

\`\`\`r
# plotDemographics.R:219
onePlotConfigIdentifier <- copy(onePlotConfig) %>% ...

# ospsuite_plotTimeprofile.R:437
dtSimulated <- data.table::copy(dtSimulated) %>% ...
\`\`\`

### Problems
- \`copy()\` creates full deep copies of data.tables
- Memory overhead scales with data size
- Some uses may be unnecessary defensive programming
- Can significantly impact performance with large datasets

## Solution

For each \`copy()\` operation:

1. **Determine if necessary** - Does the original object need to be preserved?
2. **Use in-place modification** with \`:=\` operator when safe
3. **Document reasoning** when copy is necessary
4. **Consider reference semantics** of data.table

### Decision Framework

\`\`\`r
# Copy IS necessary when:
# - Original data must be preserved for later use
# - Function returns data that may be modified by caller
# - Working with data from another package's environment

# Copy NOT necessary when:
# - Data is temporary/intermediate
# - No other references exist to the data
# - Using := for modifications
\`\`\`

## Implementation Tasks

- [ ] Audit all 14+ copy() instances across affected files
- [ ] For each instance, document the purpose and necessity of the copy
- [ ] Identify copies that can be safely removed
- [ ] Refactor to use in-place modifications where safe
- [ ] Add explanatory comments for necessary copies
- [ ] Test thoroughly for any reference-related bugs
- [ ] Benchmark memory usage before and after
- [ ] Benchmark performance improvements
- [ ] Verify all existing unit tests pass

## Expected Impact

**Estimated Performance Improvement:**
- 15-40% reduction in memory usage
- 10-25% faster execution
- Impact scales with data size

## File Locations

Multiple files - see affected files list above

## Testing Instructions

\`\`\`r
# For each modified copy() operation:

# 1. Memory profiling
library(profmem)

test_data <- # ... create large test data

# Before
p_before <- profmem({
  result <- copy(test_data) %>% process()
})

# After
p_after <- profmem({
  result <- process_inplace(test_data)
})

# Compare memory allocations
total(p_before)
total(p_after)

# 2. Verify no unwanted side effects
original <- copy(test_data)
result <- modified_function(test_data)
# Ensure original is unchanged when it should be
identical(original, test_data) # Should be TRUE if copy was necessary
\`\`\`

## Labels

performance, enhancement`,
    labels: ['performance', 'enhancement']
  },
  {
    title: 'Performance: Audit and optimize lapply with anonymous functions',
    body: `## Problem

Multiple files contain \`lapply()\` calls with anonymous functions that could be optimized. Over 20 instances were identified across the codebase.

### Affected Files
- \`R/utilities-xlsx.R\` (lines 290, 349, 411, 421)
- \`R/plotDemographics.R\` (lines 536, 597)
- \`R/PlotDataTimeProfile.R\` (lines 299, 922, 1097)
- \`R/plotSensitivity.R\` (lines 146, 267)
- \`R/utilities-data.R\` (lines 182, 703, 976, 984, 1005, 1009, 1071)
- \`R/utilities-pkParameter.R\` (line 200)
- \`R/utilities-timeprofile.R\` (line 88)

### Example Issues

\`\`\`r
# utilities-xlsx.R:290
new = unlist(lapply(names(dt), function(x) { ... }))

# utilities-data.R:1005
columnISUnique <- dataObserved[, lapply(.SD, function(x) length(unique(x))), ...]
\`\`\`

### Problems
- Anonymous functions in \`lapply()\` create overhead
- Many instances could be replaced with vectorized operations
- Some use cases mix lapply with data.table operations inefficiently
- Function lookup and call overhead for each iteration

## Solution

This is an audit task requiring case-by-case analysis:

1. **Replace with vectorized operations** where possible
2. **Use named functions** to reduce overhead for complex operations
3. **Consider vapply()** with explicit return types for better performance
4. **Use data.table native operations** instead of lapply when working with data.tables

### Example Optimizations

\`\`\`r
# Before: utilities-data.R:1005
columnISUnique <- dataObserved[, lapply(.SD, function(x) length(unique(x))), ...]

# After: Use data.table's uniqueN
columnISUnique <- dataObserved[, lapply(.SD, uniqueN), ...]

# Before: Using lapply with anonymous function
result <- lapply(data_list, function(x) x$value * 2)

# After: If vectorizable
result <- data_list$value * 2

# Before: lapply with complex function
lapply(data, function(x) complex_operation(x, param1, param2))

# After: Named function for reuse
complex_op <- function(x) complex_operation(x, param1, param2)
lapply(data, complex_op)
\`\`\`

## Implementation Tasks

- [ ] Audit all 20+ lapply instances across affected files
- [ ] Categorize by optimization opportunity (vectorize, named function, vapply, data.table)
- [ ] Create specific optimization plan for each category
- [ ] Prioritize by performance impact (based on data size and execution frequency)
- [ ] Implement optimizations in priority order
- [ ] Add benchmarks for critical paths
- [ ] Test thoroughly after each change
- [ ] Document optimization rationale in code comments
- [ ] Verify all existing unit tests pass

## Expected Impact

**Estimated Performance Improvement:** 10-30% reduction in execution time for affected operations

The impact varies significantly depending on:
- Data size
- Complexity of the function
- Frequency of execution

## File Locations

Multiple files - see affected files list above

## Testing Instructions

\`\`\`r
# For each optimized lapply:

# 1. Create representative test data
test_data <- # ... based on actual use case

# 2. Benchmark
microbenchmark(
  original = lapply(test_data, function(x) ...),
  optimized = optimized_approach(test_data),
  times = 100
)

# 3. Verify correctness
all.equal(original_result, optimized_result)

# 4. Profile if needed
profvis({
  replicate(100, optimized_approach(test_data))
})
\`\`\`

## Labels

performance, enhancement`,
    labels: ['performance', 'enhancement']
  },
  {
    title: 'Performance: Standardize on data.table syntax to avoid dplyr/data.table mixing',
    body: `## Problem

Multiple files mix dplyr and data.table syntax, causing unnecessary conversions and performance overhead. 20+ instances identified.

### Affected Files
- \`R/utilities-xlsx.R\` (line 507)
- \`R/plotSensitivity.R\` (line 229)
- \`R/plotPKBoxwhisker.R\` (lines 131, 230)
- \`R/utilities-pkParameter.R\` (lines 240, 269, 270, 358, 361, 376, 447)
- \`R/utilities-timeprofile.R\` (lines 32, 53, 57, 144, 146, 148, 149, 155, 158)

### Example Issues

\`\`\`r
# utilities-timeprofile.R:32
getSimulatedTimeprofile(...) %>%
  dplyr::mutate(scenario = scenarioName)

# utilities-pkParameter.R:269-270
dplyr::mutate(value = value * unitFactor) %>%
dplyr::select(...)

# plotPKBoxwhisker.R:131
if (dplyr::n_distinct(plotData$plotTag) > 1) nFacetColumns <- 1
\`\`\`

### Problems
- Mixing dplyr and data.table causes implicit conversions
- Each pipe operation may create intermediate copies
- dplyr operations don't use data.table's reference semantics
- Performance overhead from method dispatch
- data.table's native syntax is faster for most operations

## Solution

Standardize on data.table syntax for performance-critical code:

### Example Conversions

\`\`\`r
# Before: dplyr::mutate
dt %>% dplyr::mutate(new_col = old_col * 2)

# After: data.table :=
dt[, new_col := old_col * 2]

# Before: dplyr::select
dt %>% dplyr::select(col1, col2, col3)

# After: data.table column selection
dt[, .(col1, col2, col3)]

# Before: dplyr::n_distinct
dplyr::n_distinct(dt$column)

# After: data.table uniqueN
uniqueN(dt$column)

# Before: dplyr::filter
dt %>% dplyr::filter(value > 10)

# After: data.table subset
dt[value > 10]
\`\`\`

### Strategy

1. **Keep dplyr** for:
   - One-off data prep where performance is not critical
   - Code that heavily benefits from readability
   - Interactive analysis scripts

2. **Use data.table** for:
   - Functions called repeatedly
   - Operations on large datasets
   - Performance-critical paths
   - Operations modifying data in place

## Implementation Tasks

- [ ] Audit all dplyr/data.table mixing instances across affected files
- [ ] Identify performance-critical paths where mixing occurs
- [ ] Create conversion guide for common dplyr -> data.table patterns
- [ ] Convert high-impact operations to data.table syntax
- [ ] Test for correctness after each conversion
- [ ] Benchmark performance improvements
- [ ] Update coding standards documentation
- [ ] Verify all existing unit tests pass
- [ ] Document the rationale for syntax choice in complex cases

## Expected Impact

**Estimated Performance Improvement:** 15-35% faster data manipulation operations

Benefits include:
- Reduced memory allocations
- Fewer object copies
- Better use of data.table's optimizations
- More consistent codebase

## File Locations

Multiple files - see affected files list above

## Additional Notes

This task should be coordinated with the copy() audit and merge optimization issues for maximum impact.

## Testing Instructions

\`\`\`r
# For each conversion:

# 1. Create representative test data
test_data <- data.table(
  col1 = 1:10000,
  col2 = rnorm(10000),
  col3 = sample(letters, 10000, replace = TRUE)
)

# 2. Benchmark
microbenchmark(
  dplyr_version = test_data %>% dplyr::mutate(new = col1 * col2) %>% dplyr::filter(col1 > 5000),
  dt_version = test_data[col1 > 5000][, new := col1 * col2],
  times = 100
)

# 3. Verify identical results
all.equal(dplyr_result, dt_result, check.attributes = FALSE)
\`\`\`

## Labels

performance, enhancement`,
    labels: ['performance', 'enhancement']
  }
];

/**
 * Create a single issue using GitHub GraphQL API
 */
function createIssue(token, issue) {
  return new Promise((resolve, reject) => {
    // First, we need to get the repository ID
    const repoQuery = JSON.stringify({
      query: `
        query {
          repository(owner: "${REPO_OWNER}", name: "${REPO_NAME}") {
            id
          }
        }
      `
    });

    const repoOptions = {
      hostname: 'api.github.com',
      path: '/graphql',
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${token}`,
        'User-Agent': 'Performance-Issues-Script',
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(repoQuery)
      }
    };

    const repoReq = https.request(repoOptions, (res) => {
      let data = '';
      res.on('data', (chunk) => data += chunk);
      res.on('end', () => {
        try {
          const result = JSON.parse(data);
          if (result.errors) {
            reject(new Error(`Failed to get repository ID: ${JSON.stringify(result.errors)}`));
            return;
          }

          const repoId = result.data.repository.id;

          // Now create the issue
          const createMutation = JSON.stringify({
            query: `
              mutation CreateIssue($repositoryId: ID!, $title: String!, $body: String!) {
                createIssue(input: {
                  repositoryId: $repositoryId,
                  title: $title,
                  body: $body
                }) {
                  issue {
                    id
                    number
                    url
                  }
                }
              }
            `,
            variables: {
              repositoryId: repoId,
              title: issue.title,
              body: issue.body
            }
          });

          const createOptions = {
            hostname: 'api.github.com',
            path: '/graphql',
            method: 'POST',
            headers: {
              'Authorization': `Bearer ${token}`,
              'User-Agent': 'Performance-Issues-Script',
              'Content-Type': 'application/json',
              'Content-Length': Buffer.byteLength(createMutation)
            }
          };

          const createReq = https.request(createOptions, (createRes) => {
            let createData = '';
            createRes.on('data', (chunk) => createData += chunk);
            createRes.on('end', () => {
              try {
                const createResult = JSON.parse(createData);
                if (createResult.errors) {
                  reject(new Error(`Failed to create issue: ${JSON.stringify(createResult.errors)}`));
                  return;
                }
                resolve(createResult.data.createIssue.issue);
              } catch (error) {
                reject(error);
              }
            });
          });

          createReq.on('error', reject);
          createReq.write(createMutation);
          createReq.end();

        } catch (error) {
          reject(error);
        }
      });
    });

    repoReq.on('error', reject);
    repoReq.write(repoQuery);
    repoReq.end();
  });
}

/**
 * Main function to create all issues
 */
async function main() {
  const token = process.argv[2];

  if (!token) {
    console.error('Usage: node create-performance-issues.js <GITHUB_TOKEN>');
    console.error('\nPlease provide a GitHub Personal Access Token with repo scope.');
    console.error('\nTo get a token:');
    console.error('1. Go to https://github.com/settings/tokens');
    console.error('2. Click "Generate new token (classic)"');
    console.error('3. Select the "repo" scope');
    console.error('4. Generate and copy the token');
    process.exit(1);
  }

  console.log(`Creating ${issues.length} performance optimization issues...\n`);

  const results = [];

  for (const issue of issues) {
    try {
      console.log(`Creating: ${issue.title}`);
      const result = await createIssue(token, issue);
      results.push(result);
      console.log(`  ✓ Created issue #${result.number}: ${result.url}\n`);

      // Add a small delay to avoid rate limiting
      await new Promise(resolve => setTimeout(resolve, 1000));
    } catch (error) {
      console.error(`  ✗ Failed to create issue: ${error.message}\n`);
      results.push({ error: error.message, title: issue.title });
    }
  }

  console.log('\n=== Summary ===');
  const successful = results.filter(r => !r.error).length;
  const failed = results.filter(r => r.error).length;
  console.log(`Successfully created: ${successful} issues`);
  console.log(`Failed: ${failed} issues`);

  if (successful > 0) {
    console.log('\nCreated issues:');
    results.filter(r => !r.error).forEach(r => {
      console.log(`  - Issue #${r.number}: ${r.url}`);
    });
  }

  if (failed > 0) {
    console.log('\nFailed issues:');
    results.filter(r => r.error).forEach(r => {
      console.log(`  - ${r.title}: ${r.error}`);
    });
  }
}

// Run if called directly
if (require.main === module) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

module.exports = { createIssue, issues };
