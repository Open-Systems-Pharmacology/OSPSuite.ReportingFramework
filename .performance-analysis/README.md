# Performance Optimization Issues

## Overview

This directory contains detailed performance optimization recommendations for the OSPSuite.ReportingFramework R package, generated from a comprehensive performance analysis conducted on **2026-03-03**.

## Summary

The analysis identified **6 major optimization opportunities** with estimated performance improvements ranging from **5% to 90%**.

### Quick Statistics

- **Total issues**: 6
- **High priority (P1-P2)**: 2 issues (50-90% and 30-50% improvements)
- **Medium priority (P3-P4)**: 2 issues (20-40% and 10-30% improvements)
- **Low priority (P5-P6)**: 2 issues (5-15% and 5-10% improvements)

## Priority Matrix

| Priority | Issue | Estimated Impact | Complexity | File |
|----------|-------|------------------|------------|------|
| P1 | Replace rbind-in-loop with rbindlist | 50-90% | Medium | 01-rbind-in-loop-optimization.md |
| P2 | Vectorize unit conversion operations | 30-50% | Medium | 02-vectorize-unit-conversion.md |
| P3 | Cache and batch Excel workbook operations | 20-40% | High | 03-cache-workbook-operations.md |
| P4 | Optimize parallel processing overhead | 10-30% | Medium | 04-optimize-parallel-processing.md |
| P5 | Optimize gender conversion with fcase | 5-15% | Low | 05-optimize-gender-conversion.md |
| P6 | Optimize data.table column operations | 5-10% | Low | 06-optimize-datatable-operations.md |

## How to Create GitHub Issues

### Option 1: Automated Creation with Python Script

The `create_github_issues.py` script automates issue creation using the GitHub API.

**Prerequisites:**
- Python 3.x
- GitHub personal access token with `repo` scope
- Optional: `requests` library (falls back to `urllib` if not available)

**Usage:**

```bash
# Set your GitHub token
export GITHUB_TOKEN="your_github_token_here"

# Run the script (from this directory)
python3 create_github_issues.py

# Or run in dry-run mode to preview
python3 create_github_issues.py --dry-run

# Specify different repository
python3 create_github_issues.py --repo-owner YourOrg --repo-name YourRepo
```

**Script options:**
- `--dry-run`: Preview what would be created without actually creating issues
- `--repo-owner`: Repository owner (default: Open-Systems-Pharmacology)
- `--repo-name`: Repository name (default: OSPSuite.ReportingFramework)
- `--issues-dir`: Directory containing issue markdown files (default: current directory)

### Option 2: Using GitHub CLI

If you have `gh` CLI installed:

```bash
# Navigate to this directory
cd .performance-analysis

# Create each issue
for file in 0[1-6]-*.md; do
    title=$(head -n 1 "$file" | sed 's/^# //')
    body=$(tail -n +3 "$file")
    gh issue create --title "$title" --body "$body" --label "performance,enhancement"
done
```

### Option 3: Manual Creation via GitHub Web Interface

1. Navigate to: https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/issues/new
2. For each markdown file (01-06):
   - Copy the title (first line without `#`)
   - Copy the body (everything after the first line)
   - Add labels: `performance`, `enhancement`
   - Click "Submit new issue"

## Implementation Recommendations

### Phase 1: Quick Wins (1-2 weeks)

Start with high-impact, low-complexity optimizations:

1. **P1: rbind-in-loop → rbindlist** (01-rbind-in-loop-optimization.md)
   - Highest impact (50-90% speedup)
   - Medium complexity
   - Affects: R/utilities-data.R, R/utilities-timeprofile.R, R/plotDemographics.R

2. **P5: Gender conversion with fcase** (05-optimize-gender-conversion.md)
   - Low complexity, immediate benefit
   - Good first issue for new contributors
   - Affects: R/utilities-data.R

### Phase 2: Medium Impact (2-4 weeks)

Focus on vectorization and code quality:

3. **P2: Vectorize unit conversions** (02-vectorize-unit-conversion.md)
   - Good impact (30-50%)
   - Requires careful testing
   - Affects: R/utilities-pkParameter.R, R/utilities-timeprofile.R

4. **P6: General data.table optimizations** (06-optimize-datatable-operations.md)
   - Multiple small improvements
   - Improves code quality
   - Affects: Multiple files

### Phase 3: Complex Optimizations (4-6 weeks)

Tackle architectural improvements:

5. **P3: Workbook caching** (03-cache-workbook-operations.md)
   - Requires API changes
   - Significant I/O improvements
   - Affects: R/utilities-data.R, R/utilities-populations.R

6. **P4: Smart parallelization** (04-optimize-parallel-processing.md)
   - Needs benchmarking and testing
   - Platform-specific considerations
   - Affects: R/utilities-populations.R

## Testing Strategy

For all optimizations:

1. **Correctness Testing**
   - Verify output matches original implementation exactly
   - Use `testthat::expect_equal()` for numerical comparisons
   - Test with `all.equal()` for floating-point comparisons

2. **Performance Benchmarking**
   ```r
   library(bench)

   bench::mark(
     before = original_function(data),
     after = optimized_function(data),
     check = TRUE,  # Verify results are identical
     iterations = 100
   )
   ```

3. **Regression Testing**
   - Ensure all existing tests pass
   - Run full test suite: `devtools::test()`
   - Check R CMD check: `devtools::check()`

4. **Edge Case Testing**
   - Empty data
   - Single row/column
   - Large datasets (stress testing)
   - NA values and missing data

## Validation and Monitoring

After implementing optimizations:

1. **Profile real workflows** using `profvis`:
   ```r
   profvis::profvis({
     # Run typical workflow
   })
   ```

2. **Create benchmark suite** for ongoing monitoring:
   ```r
   # tests/benchmarks/performance_suite.R
   bench::press(
     n = c(100, 1000, 10000),
     {
       bench::mark(
         operation = your_function(data)
       )
     }
   )
   ```

3. **Document improvements** in NEWS.md:
   ```
   # Version X.Y.Z

   ## Performance Improvements

   * Significant speedup in data loading (50-90%) by replacing rbind-in-loop (#123)
   * Vectorized unit conversion operations for better performance (#124)
   ```

## Architecture Decisions

### Why these optimizations?

1. **rbind-in-loop** (P1): Classic R anti-pattern with quadratic complexity
2. **Vectorization** (P2): R's strength is vectorized operations
3. **I/O caching** (P3): File I/O is expensive, reduce unnecessary reads
4. **Smart parallelization** (P4): Overhead can exceed benefits for small tasks
5. **fcase vs ifelse** (P5): Single-pass evaluation is more efficient
6. **data.table idioms** (P6): Use data.table's optimized operations

### Trade-offs

- **Code complexity**: Some optimizations add complexity (e.g., caching)
- **Maintainability**: Balance performance with readability
- **Testing overhead**: More code paths require more tests
- **Backward compatibility**: API changes may affect users

## Additional Resources

### R Performance

- [Advanced R - Performance](https://adv-r.hadley.nz/perf-improve.html)
- [Efficient R Programming](https://csgillespie.github.io/efficientR/)
- [R Inferno](https://www.burns-stat.com/pages/Tutor/R_inferno.pdf) - Common pitfalls

### data.table

- [data.table documentation](https://rdatatable.gitlab.io/data.table/)
- [data.table performance guide](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
- [data.table reference semantics](https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html)

### Profiling and Benchmarking

- [profvis package](https://rstudio.github.io/profvis/)
- [bench package](https://bench.r-lib.org/)
- [microbenchmark package](https://cran.r-project.org/web/packages/microbenchmark/)

## Contributing

When implementing these optimizations:

1. **One issue per PR**: Keep changes focused and reviewable
2. **Include benchmarks**: Show performance improvements
3. **Add tests**: Ensure correctness is maintained
4. **Update documentation**: Reflect any API changes
5. **Follow style guide**: Maintain code consistency

## Questions or Feedback

For questions about these optimizations:
- Open a discussion in the repository
- Reference the specific issue number (once created)
- Consult the detailed markdown files in this directory

---

*Performance analysis conducted on 2026-03-03 by automated code analysis agent*
