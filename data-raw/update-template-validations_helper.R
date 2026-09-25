#!/usr/bin/env Rscript
# Helper functions for updating Excel data validation in template workbooks.
#
# This module provides utility functions to:
#  - Parse YAML configuration files defining validation rules
#  - Apply list-based data validations to Excel columns
#  - Remove or clear existing validations
#  - Generate Excel-compatible range names for validation lists
#
# All functions work with the openxlsx package's workbook objects.

excelMaxRow <- 1048576L # Excel's maximum row number
validitySheet <- "_Validity" # Hidden sheet for storing validation list values

# Error handling utility that stops execution with a message
# @param ... Message components to concatenate
abortWithMessage <- function(...) {
  stop(paste0(...), call. = FALSE)
}

# Null coalescing operator: returns y if x is NULL, otherwise x
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Generate a valid Excel named range identifier.
# Excel names can only contain letters, digits, underscores, and dots.
# They cannot start with a digit or resemble a cell reference.
# @param nameParts Character vector of name fragments to combine
# @return Character string suitable as an Excel named range
makeRangeName <- function(nameParts) {
  cleanName <- function(x) {
    x <- gsub("[^A-Za-z0-9_.]", "_", x)
    x <- gsub("_+", "_", x)
    x <- sub("^[^A-Za-z_]+", "_", x)
    x
  }

  paste0(
    "validity_",
    paste(vapply(nameParts, cleanName, character(1L)), collapse = "_")
  )
}

# Validate and convert values to character vector for Excel list validation.
# Ensures values are non-empty, non-null, and unique.
# @param values Atomic vector of values to validate
# @param context String describing the context (for error messages)
# @return Character vector of validated, unique values
# @throws Error if values are invalid
asCharacterValues <- function(values, context) {
  if (!is.atomic(values) || is.null(values) || length(values) == 0L) {
    abortWithMessage(context, ": 'values' must be a non-empty YAML list.")
  }

  values <- as.character(unlist(values, use.names = FALSE))
  if (anyNA(values) || any(!nzchar(values))) {
    abortWithMessage(context, ": validation values must not be blank or null.")
  }
  if (anyDuplicated(values)) {
    abortWithMessage(
      context,
      ": validation values must be unique (case-sensitive)."
    )
  }

  values
}


# Resolve validation values from either an inline YAML list or a named helper function.
# Exactly one of values or valuesFunction must be configured for validity: list.
# @param ruleConfig YAML rule configuration for one header
# @param context String describing the context (for error messages)
# @return Character vector of validated, unique values
resolveValidationValues <- function(ruleConfig, context) {
  inlineValues <- ruleConfig[["values", exact = TRUE]]
  valuesFunctionName <- ruleConfig[["valuesFunction", exact = TRUE]]
  hasInlineValues <- !is.null(inlineValues)
  hasValuesFunction <- !is.null(valuesFunctionName)

  if (hasInlineValues == hasValuesFunction) {
    abortWithMessage(
      context,
      ": specify exactly one of 'values' or 'valuesFunction' when 'validity: list'."
    )
  }

  if (hasInlineValues) {
    return(asCharacterValues(inlineValues, context))
  }

  valuesFunctionName <- as.character(valuesFunctionName)
  if (length(valuesFunctionName) != 1L || !nzchar(valuesFunctionName)) {
    abortWithMessage(
      context,
      ": 'valuesFunction' must be a non-empty function name."
    )
  }

  if (!exists(valuesFunctionName, mode = "function", inherits = TRUE)) {
    abortWithMessage(
      context,
      ": unknown valuesFunction '",
      valuesFunctionName,
      "'."
    )
  }

  valuesFunction <- get(valuesFunctionName, mode = "function", inherits = TRUE)
  asCharacterValues(valuesFunction(), context)
}

# Parse YAML configuration file and convert to validation rules.
# Returns one rule record per configured column header across all workbooks and sheets.
# Each column must explicitly declare one of:
#  - validity: list   (with exactly one of values: or valuesFunction:)
#  - validity: none   (to remove existing validation)
#  - validity: mixed  (to preserve existing validation)
# @param yamlFile Path to the YAML configuration file
# @return List of validation rule records, each containing:
#   - workbookName: Name of the Excel file (must end in .xlsx)
#   - sheetName: Name of the worksheet
#   - headerName: Name of the column header (exact, case-sensitive match)
#   - validityType: One of "list", "none", or "mixed"
#   - values: Character vector of validation list values (for type="list")
#   - prompt: Custom input prompt (for type="list")
#   - error: Custom error message (for type="list")
# @throws Error if YAML structure is invalid or rules are inconsistent
readValidityConfig <- function(yamlFile) {
  config <- yaml::read_yaml(yamlFile)

  if (
    !is.list(config) || is.null(config$workbooks) || !is.list(config$workbooks)
  ) {
    abortWithMessage("YAML must contain a top-level 'workbooks:' mapping.")
  }
  if (
    is.null(names(config$workbooks)) || any(!nzchar(names(config$workbooks)))
  ) {
    abortWithMessage(
      "Each entry beneath 'workbooks:' must be named with an .xlsx file name."
    )
  }

  records <- list()
  for (workbookName in names(config$workbooks)) {
    workbookConfig <- config$workbooks[[workbookName]]
    sheetConfigs <- workbookConfig$sheets %||% list()

    if (!is.list(sheetConfigs) || is.null(names(sheetConfigs))) {
      abortWithMessage(
        "Workbook '",
        workbookName,
        "': 'sheets' must be a mapping."
      )
    }

    for (sheetName in names(sheetConfigs)) {
      headerConfigs <- sheetConfigs[[sheetName]]
      if (!is.list(headerConfigs) || is.null(names(headerConfigs))) {
        abortWithMessage(
          "Workbook '",
          workbookName,
          "', sheet '",
          sheetName,
          "': headers must be a mapping."
        )
      }

      for (headerName in names(headerConfigs)) {
        ruleConfig <- headerConfigs[[headerName]]
        context <- paste0(
          "Workbook '",
          workbookName,
          "', sheet '",
          sheetName,
          "', header '",
          headerName,
          "'"
        )

        validityValue <- ruleConfig[["validity", exact = TRUE]]
        promptValue <- ruleConfig[["prompt", exact = TRUE]]
        errorValue <- ruleConfig[["error", exact = TRUE]]
        inlineValues <- ruleConfig[["values", exact = TRUE]]
        valuesFunctionName <- ruleConfig[["valuesFunction", exact = TRUE]]

        if (!is.list(ruleConfig) || is.null(validityValue)) {
          abortWithMessage(
            context,
            ": explicitly set 'validity: list', 'validity: none', or 'validity: mixed'."
          )
        }

        validityType <- as.character(validityValue)
        if (
          length(validityType) != 1L ||
            !validityType %in% c("list", "none", "mixed")
        ) {
          abortWithMessage(
            context,
            ": 'validity' must be exactly 'list', 'none', or 'mixed'."
          )
        }

        values <- character()
        prompt <- NULL
        error <- NULL
        if (identical(validityType, "list")) {
          values <- resolveValidationValues(ruleConfig, context)
          prompt <- as.character(
            promptValue %||% paste(values, collapse = ", ")
          )
          error <- as.character(
            errorValue %||%
              "Select a listed value or enter another value. Blank is allowed."
          )
        } else if (!is.null(inlineValues) || !is.null(valuesFunctionName)) {
          abortWithMessage(
            context,
            ": 'values' and 'valuesFunction' are only permitted when 'validity: list'."
          )
        }

        records[[length(records) + 1L]] <- list(
          workbookName = workbookName,
          sheetName = sheetName,
          headerName = headerName,
          validityType = validityType,
          values = values,
          valuesFunctionName = valuesFunctionName,
          rangeName = if (identical(validityType, "list")) {
            if (!is.null(valuesFunctionName)) {
              makeRangeName(valuesFunctionName)
            } else {
              makeRangeName(c(sheetName, headerName))
            }
          } else {
            NULL
          },
          prompt = prompt,
          error = error
        )
      }
    }
  }

  records
}

# Locate and return the column index for a header in a worksheet.
# Performs an exact, case-sensitive match against row 1.
# @param workbook openxlsx workbook object
# @param sheetName Name of the worksheet
# @param headerName Name of the header column to find
# @return Numeric column index
# @throws Error if header is not found or multiple matches exist
getHeaderColumn <- function(workbook, sheetName, headerName) {
  headerData <- openxlsx::readWorkbook(
    workbook,
    sheet = sheetName,
    rows = 1L,
    colNames = FALSE,
    check.names = FALSE
  )
  headers <- as.character(unlist(headerData[1L, ], use.names = FALSE))
  matches <- which(!is.na(headers) & headers == headerName) # exact and case-sensitive

  if (length(matches) == 0L) {
    abortWithMessage(
      "Sheet '",
      sheetName,
      "' has no exact, case-sensitive header named '",
      headerName,
      "'."
    )
  }
  if (length(matches) > 1L) {
    abortWithMessage(
      "Sheet '",
      sheetName,
      "' has multiple columns named '",
      headerName,
      "'. Target headers must be unique."
    )
  }

  matches
}

# Extract all non-empty header names from row 1 of a worksheet.
# @param workbook openxlsx workbook object
# @param sheetName Name of the worksheet
# @return Character vector of header names
getSheetHeaders <- function(workbook, sheetName) {
  headerData <- openxlsx::readWorkbook(
    workbook,
    sheet = sheetName,
    rows = 1L,
    colNames = FALSE,
    check.names = FALSE
  )
  headers <- as.character(unlist(headerData[1L, ], use.names = FALSE))
  headers[!is.na(headers) & nzchar(headers)]
}

# Issue warnings for columns in a sheet that have no validity configuration.
# Helps identify which columns were intentionally not configured.
# @param workbook openxlsx workbook object
# @param workbookName Name of the workbook (for messages)
# @param sheetName Name of the worksheet
# @param rules List of validation rules for this sheet
warnAboutUncoveredColumns <- function(
  workbook,
  workbookName,
  sheetName,
  rules
) {
  workbookHeaders <- getSheetHeaders(workbook, sheetName)
  configuredHeaders <- vapply(rules, `[[`, character(1L), "headerName")
  uncoveredHeaders <- setdiff(workbookHeaders, configuredHeaders)

  if (length(uncoveredHeaders) > 0L) {
    warning(
      "Workbook '",
      workbookName,
      "', sheet '",
      sheetName,
      "': no validity configuration for column(s): ",
      paste(uncoveredHeaders, collapse = ", "),
      call. = FALSE
    )
  }
}

# Convert a numeric column index to Excel column letter(s).
# @param colIndex Numeric column index (1 = A, 26 = Z, 27 = AA, etc.)
# @return Character string with column letter(s)
# @examples
# colIndexToLetter(1)  # "A"
# colIndexToLetter(27) # "AA"
colIndexToLetter <- function(colIndex) {
  letters <- LETTERS
  result <- ""
  while (colIndex > 0L) {
    colIndex <- colIndex - 1L
    result <- paste0(letters[(colIndex %% 26L) + 1L], result)
    colIndex <- colIndex %/% 26L
  }
  result
}

# Remove all data validation from a column in a worksheet.
# Clears the validation definition completely by removing it from the worksheet's validation list.
# @param workbook openxlsx workbook object
# @param sheetName Name of the worksheet
# @param columnIndex Numeric column index to clear
# @details
#   Completely removes validation from a column by deleting the validation definition.
#   A column with validity: none should allow all values without restrictions.
clearColumnValidation <- function(workbook, sheetName, columnIndex) {
  sheetIdx <- which(openxlsx::sheets(workbook) == sheetName)
  if (length(sheetIdx) == 0L) {
    abortWithMessage("Sheet '", sheetName, "' not found in workbook.")
  }

  ws <- workbook$worksheets[[sheetIdx]]
  colLetter <- colIndexToLetter(columnIndex)

  # Build a regex pattern that matches this column's references exactly
  # Pattern: column letter followed by digits (row numbers), optionally with colon range
  # E.g., for column A: matches "A1", "A2:A5", etc., but not "AA1" or "BA1"
  colPattern <- paste0("(^|:)", colLetter, "[0-9]")
  if (!is.null(ws$dataValidationsLst) && length(ws$dataValidationsLst) > 0L) {
    # Keep only validations that DON'T apply to this column
    ws$dataValidationsLst <- ws$dataValidationsLst[
      !grepl(colPattern, ws$dataValidationsLst)
    ]
  }
  if (!is.null(ws$dataValidations) && length(ws$dataValidations) > 0L) {
    ws$dataValidations <- ws$dataValidations[
      !grepl(colPattern, ws$dataValidations)
    ]
  }
}

# Apply a list-based validation rule to a column in a worksheet.
# Stores validation values in a hidden worksheet and creates an Excel named range.
# @param workbook openxlsx workbook object
# @param rule A validation rule record (from readValidityConfig)
# @param ruleIndex Numeric index of this rule (for unique range naming)
# @param validityRow Starting row number in the _Validity sheet for storing values
# @return Next available row in the _Validity sheet for the next rule's values
# @details
#   - Creates a named range in the _Validity sheet for the validation list
#   - Applies list validation to the target column (rows 2 to Excel max)
#   - Allows blank entries and uses information-level error display
applyListValidation <- function(workbook, rule, validityRow) {
  rangeName <- rule$rangeName
  columnIndex <- getHeaderColumn(workbook, rule$sheetName, rule$headerName)

  if (is.null(rangeName) || !nzchar(rangeName)) {
    abortWithMessage(
      "Workbook '",
      rule$workbookName,
      "', sheet '",
      rule$sheetName,
      "', header '",
      rule$headerName,
      "': no range name resolved for list validation."
    )
  }

  openxlsx::dataValidation(
    workbook,
    sheet = rule$sheetName,
    cols = columnIndex,
    rows = 2L:excelMaxRow,
    type = "list",
    value = rangeName,
    allowBlank = TRUE,
    showInputMsg = TRUE,
    showErrorMsg = TRUE
  )

  validityRow
}

# Refresh all validations in a single template workbook.
# Reads configuration from parsed rules, clears existing validations,
# and applies new ones. Creates or updates the _Validity hidden sheet.
# @param templateFile Path to the workbook file to update
# @param rules List of validation rules for this workbook (from readValidityConfig)
# @details
#   - Removes and recreates the _Validity hidden sheet
#   - Warns about unconfigured sheets and columns
#   - Clears existing validations for all configured columns
#   - Applies new list validations
#   - Saves the workbook back to disk
refreshWorkbook <- function(templateFile, rules) {
  workbook <- openxlsx::loadWorkbook(templateFile)

  if (validitySheet %in% openxlsx::sheets(workbook)) {
    openxlsx::removeWorksheet(workbook, validitySheet)
  }
  openxlsx::addWorksheet(workbook, validitySheet, visible = FALSE)

  configuredSheetNames <- unique(vapply(
    rules,
    `[[`,
    character(1L),
    "sheetName"
  ))
  workbookSheetNames <- openxlsx::sheets(workbook)
  uncoveredSheetNames <- setdiff(
    workbookSheetNames,
    c(configuredSheetNames, validitySheet)
  )
  if (length(uncoveredSheetNames) > 0L) {
    warning(
      "Workbook '",
      rules[[1L]]$workbookName,
      "': no validity configuration for sheet(s): ",
      paste(uncoveredSheetNames, collapse = ", "),
      call. = FALSE
    )
  }

  for (sheetName in configuredSheetNames) {
    if (!sheetName %in% workbookSheetNames) {
      abortWithMessage(
        "Workbook '",
        rules[[1L]]$workbookName,
        "' does not contain sheet '",
        sheetName,
        "'."
      )
    }
    sheetRules <- Filter(
      function(rule) identical(rule$sheetName, sheetName),
      rules
    )
    warnAboutUncoveredColumns(
      workbook,
      rules[[1L]]$workbookName,
      sheetName,
      sheetRules
    )
  }

  # 'validity: none' removes a pre-existing rule. 'validity: mixed' is
  # deliberately skipped and produces a warning, preserving any existing rule.
  for (rule in rules) {
    columnIndex <- getHeaderColumn(workbook, rule$sheetName, rule$headerName)
    if (identical(rule$validityType, "mixed")) {
      warning(
        "Workbook '",
        rule$workbookName,
        "', sheet '",
        rule$sheetName,
        "', header '",
        rule$headerName,
        "': validity is mixed; column was skipped.",
        call. = FALSE
      )
    } else {
      clearColumnValidation(workbook, rule$sheetName, columnIndex)
    }
  }

  validityRow <- 1L
  listRules <- Filter(
    function(rule) identical(rule$validityType, "list"),
    rules
  )
  uniqueListRules <- list()
  seenRangeNames <- character()
  for (rule in listRules) {
    if (!rule$rangeName %in% seenRangeNames) {
      uniqueListRules[[length(uniqueListRules) + 1L]] <- rule
      seenRangeNames <- c(seenRangeNames, rule$rangeName)
    }
  }

  for (rule in uniqueListRules) {
    listRows <- validityRow:(validityRow + length(rule$values) - 1L)
    openxlsx::writeData(
      workbook,
      sheet = validitySheet,
      x = data.frame(value = rule$values, stringsAsFactors = FALSE),
      startCol = 1L,
      startRow = validityRow,
      colNames = FALSE
    )
    openxlsx::createNamedRegion(
      workbook,
      sheet = validitySheet,
      name = rule$rangeName,
      rows = listRows,
      cols = 1L,
      overwrite = TRUE
    )
    validityRow <- validityRow + length(rule$values) + 1L
  }

  for (rule in listRules) {
    validityRow <- applyListValidation(
      workbook,
      rule,
      validityRow
    )
  }

  openxlsx::setColWidths(
    workbook,
    sheet = validitySheet,
    cols = 1L,
    widths = 45
  )
  openxlsx::saveWorkbook(workbook, templateFile, overwrite = TRUE)
  message("Updated: ", templateFile)
}


# Value functions ------------------------

# Reusable provider functions for validation lists shared across templates.
# These functions are referenced from YAML via valuesFunction.
yesNoValues <- function() {
  c("0", "1")
}

axisScaleValues <- function() {
  c("linear, log", "linear", "log")
}

facetScaleValues <- function() {
  c("fixed", "free", "free_x", "free_y")
}

shapeValues <- function() {
  unname(unlist(ospsuite.plots::Shapes))
}

dictionaryTypeValues <- function() {
  c("identifier", "timeprofile", "biometrics", "covariate", "metadata")
}

dataclassValues <- function() {
  unlist(unname(ospsuite.reportingframework::DATACLASS[
    names(ospsuite.reportingframework::DATACLASS) != 'tpTwinPop'
  ]))
}

modeOfBinningValues <- function() {
  unlist(unname(ospsuite.plots::BINNINGMODE))
}

