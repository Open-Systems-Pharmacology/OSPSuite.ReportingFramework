# Performance Optimization Issues for OSPSuite.ReportingFramework

This directory contains scripts and documentation for creating GitHub issues related to performance optimizations identified in the OSPSuite.ReportingFramework R package.

## Overview

A comprehensive performance analysis of the R package identified **9 major optimization opportunities** across multiple categories:

1. **Unit Conversion Vectorization** (2 issues) - 50-80% potential improvement
2. **rbind Anti-pattern Elimination** (3 issues) - 70-90% potential improvement
3. **lapply Optimization** (1 issue) - 10-30% potential improvement
4. **Copy Operation Reduction** (1 issue) - 15-40% memory reduction, 10-25% speed improvement
5. **Merge Operation Optimization** (1 issue) - 20-50% potential improvement
6. **dplyr/data.table Standardization** (1 issue) - 15-35% potential improvement

## Performance Issues Summary

### High Priority Issues (70-90% improvement potential)

1. **R/utilities-data.R:50-86** - Replace `rbind()` in loop with `rbindlist()`
2. **R/utilities-timeprofile.R:14-35** - Replace `rbind()` in loop with `rbindlist()`

### Medium Priority Issues (50-80% improvement potential)

3. **R/utilities-pkParameter.R:396-405** - Vectorize unit conversion operations
4. **R/utilities-timeprofile.R:62-71** - Vectorize unit conversion operations

### Moderate Priority Issues (15-50% improvement potential)

5. **Multiple files** - Optimize merge operations with data.table syntax
6. **Multiple files** - Standardize on data.table syntax to avoid dplyr/data.table mixing
7. **Multiple files** - Audit and reduce unnecessary `copy()` operations

### Lower Priority Issues (5-30% improvement potential)

8. **Multiple files** - Audit and optimize `lapply()` with anonymous functions
9. **R/plotDemographics.R:785** - Replace `rbind()` with `rbindlist()`

## Creating GitHub Issues

Two scripts are provided to create GitHub issues via the GraphQL API:

### Option 1: Node.js Script

```bash
node create-performance-issues.js <GITHUB_TOKEN>
```

### Option 2: Python Script

```bash
python3 create-performance-issues.py <GITHUB_TOKEN>
```

### Getting a GitHub Token

1. Go to GitHub Settings → Developer settings → Personal access tokens → Tokens (classic)
2. Click "Generate new token (classic)"
3. Give it a descriptive name (e.g., "Performance Issues Creation")
4. Select the `repo` scope
5. Click "Generate token"
6. Copy the token and use it with the scripts

**Security Note:** Keep your token secure and never commit it to version control.

## Issue Details

Each issue includes:

- **Problem Description** - Current implementation with code examples
- **Issues** - Specific problems with the current approach
- **Solution** - Proposed implementation with code examples
- **Implementation Tasks** - Checklist of work items
- **Expected Impact** - Estimated performance improvement
- **File Locations** - Exact file paths and line numbers
- **Labels** - Appropriate categorization

All issues are written to be directly assignable to a coding agent, with:
- Clear problem statements
- Specific file locations and line numbers
- Code examples for both current and proposed implementations
- Actionable task checklists
- Measurable success criteria

## Implementation Guidelines

When implementing these optimizations:

1. **Start with high-priority issues** for maximum impact
2. **Profile before and after** to measure actual improvements
3. **Test thoroughly** - performance optimizations should not change behavior
4. **Benchmark on realistic data** - use actual project data when possible
5. **Document changes** - update comments and documentation
6. **Consider dependencies** - some optimizations work better together

### Recommended Implementation Order

1. Start with rbind-in-loop issues (#3, #4) - highest impact, relatively straightforward
2. Implement unit conversion vectorization (#1, #2) - high impact, good first issues
3. Optimize merge operations (#8) - medium complexity, good impact
4. Audit copy() operations (#7) - requires careful analysis
5. Standardize data.table/dplyr (#9) - larger refactoring effort
6. Optimize lapply calls (#6) - case-by-case optimization
7. Final rbind cleanup (#5) - consistency improvement

## Testing Recommendations

### Performance Testing

```r
# Benchmark template
library(microbenchmark)

# Before optimization
before <- function() {
  # Original implementation
}

# After optimization
after <- function() {
  # Optimized implementation
}

# Compare
microbenchmark(
  before = before(),
  after = after(),
  times = 100
)
```

### Correctness Testing

1. **Unit tests** - Ensure existing tests pass
2. **Integration tests** - Test with realistic workflows
3. **Numerical accuracy** - Verify results match exactly
4. **Edge cases** - Test with empty data, single rows, large datasets

## Performance Analysis Methodology

The performance issues were identified through:

1. **Code review** - Manual inspection of R source files
2. **Pattern matching** - Searching for known anti-patterns:
   - `rbind()` in loops
   - Row-wise operations instead of vectorized
   - `apply()` on data.tables
   - Unnecessary `copy()` operations
   - Mixed dplyr/data.table usage
3. **Complexity analysis** - Identifying O(n²) algorithms
4. **Best practices review** - Comparing against data.table documentation

## Estimated Overall Impact

If all optimizations are implemented:

- **10-40% reduction** in overall execution time for typical workflows
- **20-50% reduction** in memory usage
- **70-90% improvement** for specific bottleneck operations
- Better scalability with larger datasets

Actual improvements will vary based on:
- Dataset size
- Number of scenarios/files processed
- Hardware specifications
- Specific workflow patterns

## Additional Resources

- [data.table documentation](https://rdatatable.gitlab.io/data.table/)
- [data.table wiki - Benchmarks](https://github.com/Rdatatable/data.table/wiki/Benchmarks-:-Grouping)
- [Advanced R - Performance](http://adv-r.had.co.nz/Performance.html)
- [Efficient R Programming](https://csgillespie.github.io/efficientR/)

## Support

For questions about these optimizations or the scripts:

1. Open an issue in the OSPSuite.ReportingFramework repository
2. Reference this performance analysis documentation
3. Include specific file locations and use cases

## License

These scripts and documentation are provided as part of the OSPSuite.ReportingFramework project and follow the same license terms.
