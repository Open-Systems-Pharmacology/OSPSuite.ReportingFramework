# Message Centralization Guide

This document explains the pattern for centralizing error, warning, and message strings in the `messages.R` file, following the approach used in the OSPSuite-R package.

## Overview

All error, warning, and message strings have been centralized in `R/messages.R`. This provides:
- Centralized message management and maintenance
- Easier updates to message text
- Consistent messaging across the package
- Better testability with explicit message checking

## Pattern

### 1. Define messages in R/messages.R

Messages are defined as functions in the `messages` list. Each function accepts parameters needed for dynamic content and returns the complete message string.

**Example:**
```r
messages$errorPKParameterNotDefined <- function(userPar) {
  paste("pkParameter", userPar, 'is not defined in "Userdef PK Parameter" sheet.')
}

messages$warningNoObservedDataAvailable <- function(plotName, columns) {
  paste0("For plot ", plotName, ", no observed data available for ", 
         paste(columns, collapse = ', '), ", plots will be omitted")
}
```

### 2. Use messages in source code

Replace inline strings with calls to message functions.

**Before:**
```r
stop(paste("pkParameter", userPar, 'is not defined in "Userdef PK Parameter" sheet.'))
```

**After:**
```r
stop(messages$errorPKParameterNotDefined(userPar))
```

### 3. Update tests to reference messages

Update `expect_error()` and `expect_warning()` calls to reference the centralized messages.

**Before:**
```r
expect_error(.addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter))
```

**After:**
```r
expect_error(
  .addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter), 
  messages$errorPKParameterNotDefined("DoesNotExist")
)
```

## Naming Convention

Message function names follow this pattern:
- `error<Description>` - for error messages
- `warning<Description>` - for warning messages
- `message<Description>` - for informational messages

Use descriptive camelCase names that clearly indicate the error condition.

## Files Already Updated

The following files have been updated to use centralized messages:

### Source Files:
- R/messages.R (new file with ~100+ message functions)
- R/utilities-aggregation.R
- R/utilities-plot.R (partially)
- R/utilities-pkParameter.R
- R/utilities-data.R (partially)
- R/utilities-workflow.R
- R/ospsuite_plotTimeprofile.R
- R/PlotDataTimeProfile.R
- R/RmdPlotManager.R

### Test Files:
- tests/testthat/test-utilities-aggregation.R
- tests/testthat/test-utilities-pkParameter.R
- tests/testthat/test-utilities-workflow.R
- tests/testthat/test-RmdContainer.R

## Remaining Files to Update

The following files still have inline message strings that should be migrated:

### High Priority (heavily tested):
- R/WorkflowScriptExporter.R
- R/plotTimeProfilePanels.R
- R/utilities-xlsx.R
- R/plotPKForest.R
- R/plotDemographics.R
- R/plotPKBoxwhisker.R

### Medium Priority:
- R/utilities-sensitivity.R
- R/utilities-logging.R
- R/utilities-markdown.R
- R/utilities-populations.R
- R/esqlabsR_bracket.R

## Step-by-Step Guide for Updating a File

### 1. Identify messages in source file

Search for patterns like:
```r
stop("...")
warning("...")
message("...")
stop(paste(...))
warning(paste(...))
```

### 2. Add message functions to R/messages.R

For each unique message, create a function:

```r
messages$errorYourDescriptiveName <- function(param1, param2) {
  paste("Your error message with", param1, "and", param2)
}
```

### 3. Update source file to use message functions

Replace the inline string with a call to the message function:

```r
# Before
stop(paste("Your error message with", param1, "and", param2))

# After
stop(messages$errorYourDescriptiveName(param1, param2))
```

### 4. Update corresponding test file

Find tests that use `expect_error()` or `expect_warning()` for the updated function and add the expected message:

```r
# Before
expect_error(yourFunction(badInput))

# After
expect_error(yourFunction(badInput), messages$errorYourDescriptiveName(expectedParam1, expectedParam2))
```

### 5. Verify the changes

Run the tests to ensure the error messages match:
```r
testthat::test_file("tests/testthat/test-your-file.R")
```

## Tips

1. **Use paste() or paste0()** instead of glue::glue() in message functions for consistency
2. **Keep message functions simple** - they should only construct the message string
3. **Parameters should be meaningful** - use descriptive parameter names
4. **Test the messages** - ensure tests check for the exact message text
5. **Group related messages** - organize messages by topic/file in messages.R with comments

## Example: Complete Migration

### Step 1: Identify message in source
**File: R/utilities-xlsx.R**
```r
stop(paste("Sheet", sheetName, "does not exist in the workbook."))
```

### Step 2: Add to messages.R
```r
messages$errorSheetDoesNotExist <- function(sheetName) {
  paste("Sheet", sheetName, "does not exist in the workbook.")
}
```

### Step 3: Update source file
**File: R/utilities-xlsx.R**
```r
stop(messages$errorSheetDoesNotExist(sheetName))
```

### Step 4: Update test file
**File: tests/testthat/test-utilities-xlsx.R**
```r
# Before
expect_error(xlsxCloneSheet(wb, "NonExistentSheet", "NewSheet"))

# After
expect_error(
  xlsxCloneSheet(wb, "NonExistentSheet", "NewSheet"), 
  messages$errorSheetDoesNotExist("NonExistentSheet")
)
```

## Questions?

For questions or issues, refer to:
- OSPSuite-R messages.R: https://github.com/Open-Systems-Pharmacology/OSPSuite-R/blob/main/R/messages.R
- This PR discussion

