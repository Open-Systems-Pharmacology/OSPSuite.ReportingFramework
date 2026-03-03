# Performance Optimization Issues Summary

This document contains detailed performance optimization recommendations for the OSPSuite.ReportingFramework R package. Each section represents a separate issue that should be created in the GitHub repository.

## Overview

A comprehensive performance analysis was conducted on the R package, identifying 6 major optimization opportunities with estimated performance improvements ranging from 5% to 90%.

## Priority Matrix

| Priority | Issue | Estimated Impact | Complexity |
|----------|-------|------------------|------------|
| P1 | Replace rbind-in-loop with rbindlist | 50-90% | Medium |
| P2 | Vectorize unit conversion operations | 30-50% | Medium |
| P3 | Cache and batch Excel workbook operations | 20-40% | High |
| P4 | Optimize parallel processing overhead | 10-30% | Medium |
| P5 | Optimize gender conversion with fcase | 5-15% | Low |
| P6 | Optimize data.table column operations | 5-10% | Low |

## Issue Files

1. **01-rbind-in-loop-optimization.md** - Replace rbind in loops with rbindlist
2. **02-vectorize-unit-conversion.md** - Vectorize unit conversion operations
3. **03-cache-workbook-operations.md** - Cache Excel workbook operations
4. **04-optimize-parallel-processing.md** - Add conditional parallelization
5. **05-optimize-gender-conversion.md** - Use data.table fcase for gender
6. **06-optimize-datatable-operations.md** - General data.table optimizations

## How to Create Issues

### Option 1: Using GitHub Web Interface

1. Navigate to: https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/issues/new
2. Copy the title and body from each .md file
3. Add labels: `performance`, `enhancement`
4. Create the issue

### Option 2: Using GitHub CLI (if permissions allow)

```bash
# From the repository root
for file in /tmp/performance-issues/*.md; do
  # Extract title (first line after # header)
  title=$(head -n 1 "$file" | sed 's/^# //')

  # Get body (everything after first line)
  body=$(tail -n +3 "$file")

  # Create issue
  gh issue create --title "$title" --body "$body" --label "performance,enhancement"
done
```

### Option 3: Using Python Script

See `create_github_issues.py` for automated issue creation via GitHub API.

## Implementation Recommendations

### Phase 1: Quick Wins (1-2 weeks)
- P1: rbind-in-loop → rbindlist (highest impact, medium effort)
- P5: Gender conversion with fcase (low effort, visible improvement)

### Phase 2: Medium Impact (2-4 weeks)
- P2: Vectorize unit conversions (good impact, needs testing)
- P6: General data.table optimizations (code quality + performance)

### Phase 3: Complex Optimizations (4-6 weeks)
- P3: Workbook caching (requires API changes)
- P4: Smart parallelization (needs benchmarking)

## Testing Strategy

For all optimizations:

1. **Correctness**: Verify output matches original implementation
2. **Performance**: Benchmark before/after with representative data
3. **Regression**: Ensure existing tests pass
4. **Edge cases**: Test with small, medium, and large datasets

## Monitoring and Validation

After implementing optimizations:

1. Use `profvis` package to profile real workflows
2. Create benchmark suite for ongoing monitoring
3. Document performance improvements in release notes
4. Consider adding performance regression tests

## Additional Resources

- [data.table performance guide](https://rdatatable.gitlab.io/data.table/)
- [R performance optimization](https://adv-r.hadley.nz/perf-improve.html)
- [Efficient R programming](https://csgillespie.github.io/efficientR/)

## Contact

For questions about these optimizations, refer to the detailed issue descriptions or consult the R performance best practices documentation.
