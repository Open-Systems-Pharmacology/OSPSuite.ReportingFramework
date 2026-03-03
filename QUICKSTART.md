# Quick Start: Creating Performance Optimization Issues

## TL;DR

Run one of these commands to create 9 GitHub issues for performance optimizations:

```bash
# Using Node.js
node create-performance-issues.js YOUR_GITHUB_TOKEN

# Using Python
python3 create-performance-issues.py YOUR_GITHUB_TOKEN
```

## Get a GitHub Token

1. Go to: https://github.com/settings/tokens
2. Click "Generate new token (classic)"
3. Select the `repo` scope
4. Copy the generated token

## What Gets Created

The script will create 9 detailed GitHub issues:

### High Priority (Implement First)
1. **rbind-in-loop → rbindlist** in utilities-data.R (70-90% faster)
2. **rbind-in-loop → rbindlist** in utilities-timeprofile.R (70-90% faster)

### Medium Priority (Good First Issues)
3. **Vectorize unit conversion** in utilities-pkParameter.R (50-80% faster)
4. **Vectorize unit conversion** in utilities-timeprofile.R (50-80% faster)

### Moderate Priority (Systematic Improvements)
5. **Optimize merge operations** across multiple files (20-50% faster)
6. **Standardize data.table syntax** across multiple files (15-35% faster)
7. **Reduce unnecessary copy()** operations across multiple files (15-40% less memory)

### Lower Priority (Code Quality)
8. **Optimize lapply calls** across multiple files (10-30% faster)
9. **rbind → rbindlist** in plotDemographics.R (5-10% faster)

## Expected Overall Impact

- 10-40% faster execution
- 20-50% less memory usage
- 70-90% improvement for specific operations

## Full Documentation

- **IMPLEMENTATION_GUIDE.md** - Complete implementation guide
- **PERFORMANCE_ISSUES_README.md** - Detailed technical analysis

## Script Features

✓ Creates issues with specific file locations and line numbers
✓ Includes current code and proposed solutions
✓ Provides implementation checklists
✓ Assigns appropriate labels
✓ Estimates performance impact
✓ Written for direct assignment to coding agents

## Troubleshooting

**"Invalid token"**: Ensure your GitHub token has `repo` scope

**"Permission denied"**: Verify you have write access to the repository

**"Cannot find module"**: For Node.js, ensure you have Node.js v12+ installed

**Script syntax error**: Both scripts have been validated and should work correctly

## Next Steps After Creating Issues

1. Review and prioritize the issues based on your needs
2. Assign to developers or coding agents
3. Implement in recommended order (high priority first)
4. Benchmark each change to verify improvements
5. Update this documentation with actual results

## Questions?

See the full documentation files or contact the repository maintainers.
