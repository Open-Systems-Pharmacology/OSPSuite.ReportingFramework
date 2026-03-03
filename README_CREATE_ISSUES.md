# Creating Performance Optimization Issues

This directory contains scripts to automatically create GitHub issues for performance optimization tasks identified in the OSPSuite.ReportingFramework R package.

## Overview

Based on a comprehensive performance analysis documented in `IMPLEMENTATION_GUIDE.md` on the `claude/analyze-performance-optimizations` branch, **9 performance optimization issues** will be created covering:

1. **rbind-in-loop anti-patterns** (3 issues) - 70-90% potential improvement
2. **Unit conversion vectorization** (2 issues) - 50-80% potential improvement
3. **Merge operation optimization** (1 issue) - 20-50% potential improvement
4. **copy() operation audit** (1 issue) - 15-40% memory reduction
5. **lapply optimization** (1 issue) - 10-30% potential improvement
6. **dplyr/data.table standardization** (1 issue) - 15-35% potential improvement

## Prerequisites

You need a GitHub Personal Access Token with `repo` scope:

1. Go to https://github.com/settings/tokens
2. Click **"Generate new token (classic)"**
3. Name it (e.g., "Performance Issues Creation")
4. Select the **`repo`** scope (full control of private repositories)
5. Click **"Generate token"**
6. Copy the token immediately (you won't be able to see it again)

**Security Note:** Keep your token secure and never commit it to version control.

## Usage

### Option 1: Node.js Script

```bash
node create-performance-issues.js YOUR_GITHUB_TOKEN_HERE
```

**Requirements:**
- Node.js v12 or higher
- No additional dependencies (uses built-in `https` module)

### Option 2: Python Script

```bash
python3 create-performance-issues.py YOUR_GITHUB_TOKEN_HERE
```

**Requirements:**
- Python 3.6 or higher
- No additional dependencies (uses standard library)

## What the Scripts Do

Both scripts perform identical operations:

1. Connect to GitHub's GraphQL API
2. Get the repository ID for `Open-Systems-Pharmacology/OSPSuite.ReportingFramework`
3. Create 9 detailed issues, one for each performance optimization task
4. Add a 1-second delay between issues to avoid rate limiting
5. Provide progress updates and a summary

Each issue created includes:

- **Title**: Descriptive performance optimization task
- **Problem description**: Current implementation with code examples
- **Issues identified**: Specific problems with the current approach
- **Solution**: Proposed implementation with code examples
- **Implementation tasks**: Detailed checklist for completing the work
- **Expected impact**: Estimated performance improvement
- **File locations**: Exact file paths and line numbers
- **Testing instructions**: How to benchmark and verify the optimization
- **Labels**: Appropriate categorization (performance, enhancement, etc.)

## Issues to be Created

### High Priority (70-90% improvement)

1. **Performance: Replace rbind-in-loop with rbindlist in utilities-data.R**
   - File: `R/utilities-data.R:50-86`
   - Labels: performance, enhancement

2. **Performance: Replace rbind-in-loop with rbindlist in utilities-timeprofile.R**
   - File: `R/utilities-timeprofile.R:14-35`
   - Labels: performance, enhancement

### Medium Priority (50-80% improvement)

3. **Performance: Vectorize unit conversion operations in utilities-pkParameter.R**
   - File: `R/utilities-pkParameter.R:396-405`
   - Labels: performance, enhancement, good-first-issue

4. **Performance: Vectorize unit conversion operations in utilities-timeprofile.R**
   - File: `R/utilities-timeprofile.R:62-71`
   - Labels: performance, enhancement, good-first-issue

5. **Performance: Replace rbind with rbindlist in plotDemographics.R**
   - File: `R/plotDemographics.R:785`
   - Labels: performance, enhancement, good-first-issue

### Moderate Priority (15-50% improvement)

6. **Performance: Optimize merge operations with data.table native syntax**
   - Files: Multiple (15+ instances)
   - Labels: performance, enhancement

7. **Performance: Audit and reduce unnecessary data.table copy() operations**
   - Files: Multiple (14+ instances)
   - Labels: performance, enhancement

### Lower Priority (10-35% improvement)

8. **Performance: Audit and optimize lapply with anonymous functions**
   - Files: Multiple (20+ instances)
   - Labels: performance, enhancement

9. **Performance: Standardize on data.table syntax to avoid dplyr/data.table mixing**
   - Files: Multiple (20+ instances)
   - Labels: performance, enhancement

## Expected Output

```
Creating 9 performance optimization issues...

Creating: Performance: Replace rbind-in-loop with rbindlist in utilities-data.R
  ✓ Created issue #123: https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/issues/123

Creating: Performance: Replace rbind-in-loop with rbindlist in utilities-timeprofile.R
  ✓ Created issue #124: https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/issues/124

...

=== Summary ===
Successfully created: 9 issues
Failed: 0 issues

Created issues:
  - Issue #123: https://github.com/...
  - Issue #124: https://github.com/...
  ...
```

## Troubleshooting

### Authentication Errors

If you see authentication errors:
- Verify your token has the `repo` scope
- Check that the token hasn't expired
- Ensure you copied the entire token without extra spaces

### Rate Limiting

GitHub has rate limits for API calls:
- The scripts include 1-second delays between issues
- If you hit rate limits, wait a few minutes and try again
- Authenticated requests have higher limits (5000/hour)

### Network Errors

If you encounter network issues:
- Check your internet connection
- Verify you can access api.github.com
- Check if you're behind a proxy that might block API calls

### Permission Errors

If you get permission errors:
- Ensure your token has `repo` scope
- Verify you have permission to create issues in the repository
- Check if you're a collaborator or the repository allows public issue creation

## Issue Description Format

Each issue is written to be **directly assignable to a coding agent** with:

- ✓ Clear problem statement with code examples
- ✓ Specific file locations and line numbers
- ✓ Current implementation analysis
- ✓ Proposed solution with code examples
- ✓ Detailed implementation task checklist
- ✓ Expected performance impact estimates
- ✓ Testing instructions with benchmarking templates
- ✓ Appropriate labels for filtering and prioritization

## Implementation Recommendations

### Phase 1: Quick Wins (Weeks 1-2)
Start with high-priority rbind-in-loop issues (#1, #2) and unit conversion vectorization (#3, #4) for immediate impact.

### Phase 2: Systematic Improvements (Weeks 3-4)
Address merge optimization (#6) and final rbind cleanup (#5).

### Phase 3: Larger Refactoring (Weeks 5-8)
Tackle dplyr/data.table standardization (#9), copy() audit (#7), and lapply optimization (#8).

## Estimated Overall Impact

If all optimizations are implemented:
- **10-40%** reduction in overall execution time
- **20-50%** reduction in memory usage
- **70-90%** improvement for specific bottleneck operations
- Better scalability with larger datasets

## Related Documentation

- **IMPLEMENTATION_GUIDE.md** (on `claude/analyze-performance-optimizations` branch) - Full implementation guide
- **PERFORMANCE_ISSUES_README.md** (on `claude/analyze-performance-optimizations` branch) - Detailed analysis

## License

These scripts and documentation follow the same license as the OSPSuite.ReportingFramework project.

## Support

For questions or issues:
1. Check the troubleshooting section above
2. Review error messages for specific API errors
3. Open an issue in the OSPSuite.ReportingFramework repository
4. Contact the repository maintainers
