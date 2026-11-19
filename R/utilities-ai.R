#' Export workflow context for AI tools
#'
#' Collects a compact, structured summary of key workflow configuration tables.
#' The output can be used directly by AI assistants to reduce manual prompt setup.
#'
#' @param projectConfiguration Project configuration object.
#' @param file Optional output path for JSON export. If `NULL`, no file is written.
#' @param includeNumericValues If `TRUE`, include values imported via
#'   `dataClassType = "numericValues"` when available.
#'
#' @details Experimental: this helper is intended for early evaluation and may
#'   change in future releases.
#'
#' @return A named list with workflow context information.
#' @export
#' @family project initialization
exportWorkflowContext <- function(
  projectConfiguration,
  file = "workflow-context.json",
  includeNumericValues = TRUE
) {
  lifecycle::signal_stage("experimental", "exportWorkflowContext()")
  checkmate::assertFlag(includeNumericValues)

  dataImportMapFile <- projectConfiguration$addOns$dataImportMapFile
  scenariosFile <- projectConfiguration$scenariosFile
  reportsFile <- projectConfiguration$addOns$reportsFile

  checkmate::assertFileExists(dataImportMapFile)
  checkmate::assertFileExists(scenariosFile)
  checkmate::assertFileExists(reportsFile)

  dataImportWb <- openxlsx::loadWorkbook(dataImportMapFile)
  dataFiles <- xlsxReadData(
    wb = dataImportWb,
    sheetName = "DataFiles",
    skipDescriptionRow = TRUE
  )

  context <- list(
    generatedAt = as.character(Sys.time()),
    packageVersion = as.character(utils::packageVersion(
      "ospsuite.reportingframework"
    )),
    configurationFiles = list(
      dataImportMapFile = dataImportMapFile,
      scenariosFile = scenariosFile,
      reportsFile = reportsFile
    ),
    dataClassValues = unname(unlist(DATACLASS)),
    dataFiles = list(
      fileIdentifiers = unique(dataFiles$fileIdentifier),
      dataClasses = unique(dataFiles$dataClass),
      dictionaries = unique(stats::na.omit(dataFiles$dictionary))
    )
  )

  context$outputPathIds <- .safeUniqueValues(
    getOutputPathIds(reportsFile),
    "outputPathId"
  )
  context$dataGroups <- .safeUniqueValues(getDataGroups(reportsFile), "group")
  context$scenarioNames <- .safeUniqueValues(
    getScenarioDefinitions(
      wbScenarios = scenariosFile,
      wbPlots = reportsFile
    ),
    "scenarioName"
  )

  if (includeNumericValues) {
    numericValues <- tryCatch(
      readObservedDataByDictionary(
        projectConfiguration = projectConfiguration,
        spreadData = FALSE,
        dataClassType = "numericValues"
      ),
      error = function(err) {
        return(NULL)
      }
    )

    if (!is.null(numericValues)) {
      context$numericValues <- numericValues
    }
  }

  if (!is.null(file)) {
    checkmate::assertString(file, min.chars = 1)
    jsonData <- jsonlite::toJSON(
      context,
      pretty = TRUE,
      auto_unbox = TRUE,
      digits = NA
    )
    writeLines(jsonData, file)
  }

  return(context)
}

#' Create an AI-ready workflow prompt
#'
#' Builds a project-specific prompt text that users can paste into an AI assistant.
#' The prompt includes available scenarios, outputs, groups, and data-import metadata.
#'
#' @param projectConfiguration Project configuration object.
#' @param task Intended AI task label (e.g. `"create-workflow"`).
#' @param includeExamples If `TRUE`, include example commands and snippets.
#' @param maxItemsPerSection Maximum number of list items included per section.
#'
#' @details Experimental: this helper is intended for early evaluation and may
#'   change in future releases.
#'
#' @return A character string containing the prompt.
#' @export
#' @family project initialization
createAIWorkflowPrompt <- function(
  projectConfiguration,
  task = "create-workflow",
  includeExamples = TRUE,
  maxItemsPerSection = 30
) {
  lifecycle::signal_stage("experimental", "createAIWorkflowPrompt()")
  checkmate::assertString(task, min.chars = 1)
  checkmate::assertFlag(includeExamples)
  checkmate::assertCount(maxItemsPerSection, positive = TRUE)

  context <- exportWorkflowContext(
    projectConfiguration = projectConfiguration,
    file = NULL,
    includeNumericValues = TRUE
  )

  outputPathIds <- .truncateValues(context$outputPathIds, maxItemsPerSection)
  dataGroups <- .truncateValues(context$dataGroups, maxItemsPerSection)
  scenarioNames <- .truncateValues(context$scenarioNames, maxItemsPerSection)
  fileIdentifiers <- .truncateValues(
    context$dataFiles$fileIdentifiers,
    maxItemsPerSection
  )

  lines <- c(
    "You are assisting with an OSPSuite ReportingFramework workflow.",
    "",
    paste("Task:", task),
    "",
    "Project context:",
    paste(
      "- Data import map workbook:",
      context$configurationFiles$dataImportMapFile
    ),
    paste(
      "- Scenarios workbook:",
      context$configurationFiles$scenariosFile
    ),
    paste("- Reports workbook:", context$configurationFiles$reportsFile),
    "",
    "Available file identifiers:",
    .toBulletLines(fileIdentifiers),
    "",
    "Available scenarios:",
    .toBulletLines(scenarioNames),
    "",
    "Available outputPathIds:",
    .toBulletLines(outputPathIds),
    "",
    "Available data groups:",
    .toBulletLines(dataGroups),
    "",
    "Allowed DataClass values:",
    .toBulletLines(context$dataClassValues),
    "",
    "Requirements:",
    "- Do not hard code numerical datasets in scripts.",
    "- Prefer values imported from configuration workbooks and dictionaries.",
    "- Keep workflow reproducible and validate units before use."
  )

  if (!is.null(context$numericValues)) {
    numericPreview <- context$numericValues[,
      c("variableName", "value", "unit"),
      with = FALSE
    ]
    numericPreview <- utils::head(numericPreview, maxItemsPerSection)

    lines <- c(
      lines,
      "",
      "Available numeric values:",
      .toBulletLines(apply(numericPreview, 1, function(row) {
        paste(
          row[[1]],
          "=",
          row[[2]],
          ifelse(
            is.na(row[[3]]) || row[[3]] == "",
            "",
            paste0(" ", row[[3]])
          )
        )
      }))
    )
  }

  if (includeExamples) {
    lines <- c(
      lines,
      "",
      "Example commands:",
      "- dataObserved <- readObservedDataByDictionary(projectConfiguration, dataClassType = \"timeprofile\")",
      "- dataObservedPK <- readObservedDataByDictionary(projectConfiguration, dataClassType = \"pkParameter\")",
      "- numericValues <- readObservedDataByDictionary(projectConfiguration, dataClassType = \"numericValues\")",
      "- dose <- getNumericValue(numericValues, \"Dose\", expectedUnit = \"mg\")"
    )
  }

  return(paste(lines, collapse = "\n"))
}

#' Create an AI workflow prompt from a predefined task
#'
#' Convenience wrapper around `createAIWorkflowPrompt()` with predefined
#' task labels for common user journeys.
#'
#' @param projectConfiguration Project configuration object.
#' @param task One of `"create-workflow"`, `"import-data"`, `"build-plots"`,
#'   or `"debug-run"`.
#' @param includeExamples If `TRUE`, include example commands and snippets.
#' @param maxItemsPerSection Maximum number of list items included per section.
#'
#' @details Experimental: this helper is intended for early evaluation and may
#'   change in future releases.
#'
#' @return A character string containing the prompt.
#' @export
#' @family project initialization
createAIPromptFromTask <- function(
  projectConfiguration,
  task = c("create-workflow", "import-data", "build-plots", "debug-run"),
  includeExamples = TRUE,
  maxItemsPerSection = 30
) {
  lifecycle::signal_stage("experimental", "createAIPromptFromTask()")
  task <- match.arg(task)

  taskInstruction <- switch(
    task,
    "create-workflow" = "Create or refine a complete reproducible RF workflow script.",
    "import-data" = "Focus on data import setup, dictionaries, and numeric values definitions.",
    "build-plots" = "Focus on selecting outputs and generating plot configuration and plot calls.",
    "debug-run" = "Focus on troubleshooting workflow execution, missing sheets, and data mismatches."
  )

  prompt <- createAIWorkflowPrompt(
    projectConfiguration = projectConfiguration,
    task = task,
    includeExamples = includeExamples,
    maxItemsPerSection = maxItemsPerSection
  )

  return(paste(
    prompt,
    "",
    "Task-specific guidance:",
    paste("-", taskInstruction),
    sep = "\n"
  ))
}

#' Validate workflow configuration for AI-assisted usage
#'
#' Runs lightweight checks to detect missing sheets, unknown data classes,
#' and numeric-values configuration inconsistencies before users execute
#' AI-generated workflow code.
#'
#' @param projectConfiguration Project configuration object.
#' @param strict If `TRUE`, warnings are treated as failing checks.
#'
#' @details Experimental: this helper is intended for early evaluation and may
#'   change in future releases.
#'
#' @return A list with `ok`, `errors`, `warnings`, and `suggestions`.
#' @export
#' @family project initialization
validateWorkflowForAI <- function(projectConfiguration, strict = FALSE) {
  lifecycle::signal_stage("experimental", "validateWorkflowForAI()")
  checkmate::assertFlag(strict)

  dataImportMapFile <- projectConfiguration$addOns$dataImportMapFile
  scenariosFile <- projectConfiguration$scenariosFile
  reportsFile <- projectConfiguration$addOns$reportsFile
  configurationFiles <- c(dataImportMapFile, scenariosFile, reportsFile)

  fileErrors <- .missingConfigurationFileErrors(configurationFiles)
  fileWarnings <- character()

  if (length(fileErrors) == 0) {
    workbookValidation <- .validateDataImportWorkbookForAI(dataImportMapFile)
    fileErrors <- c(fileErrors, workbookValidation$errors)
    fileWarnings <- c(fileWarnings, workbookValidation$warnings)
  }

  suggestions <- .aiValidationSuggestions(fileErrors, fileWarnings)

  errors <- unique(fileErrors)
  warnings <- unique(fileWarnings)

  ok <- length(errors) == 0 && (!strict || length(warnings) == 0)

  return(list(
    ok = ok,
    errors = unique(errors),
    warnings = unique(warnings),
    suggestions = unique(suggestions)
  ))
}

#' Collect Missing Configuration File Errors
#'
#' @param configurationFiles Character vector of required configuration file paths.
#'
#' @return Character vector with missing-file error messages.
#' @keywords internal
#' @noRd
.missingConfigurationFileErrors <- function(configurationFiles) {
  missingFiles <- configurationFiles[!file.exists(configurationFiles)]
  if (length(missingFiles) == 0) {
    return(character())
  }

  return(paste("Missing configuration file:", missingFiles))
}

#' Validate Data Import Workbook for AI Workflow Checks
#'
#' @param dataImportMapFile Path to the DataImportMap workbook.
#'
#' @return A list with `errors` and `warnings` character vectors.
#' @details
#' Performs staged checks for required sheets, required columns, allowed `dataClass`
#' values, and consistency of optional numeric-values dictionary sheets.
#' @keywords internal
#' @noRd
.validateDataImportWorkbookForAI <- function(dataImportMapFile) {
  errors <- character()
  warnings <- character()

  wb <- openxlsx::loadWorkbook(dataImportMapFile)
  requiredSheets <- c("DataFiles")
  missingSheets <- setdiff(requiredSheets, wb$sheet_names)
  if (length(missingSheets) > 0) {
    errors <- c(
      errors,
      paste(
        "Missing required sheets in DataImportMap:",
        paste(missingSheets, collapse = ", ")
      )
    )
    return(list(errors = errors, warnings = warnings))
  }

  dataFiles <- xlsxReadData(
    wb = wb,
    sheetName = "DataFiles",
    skipDescriptionRow = TRUE
  )

  requiredColumns <- c("fileIdentifier", "dataClass")
  if (!all(requiredColumns %in% names(dataFiles))) {
    errors <- c(
      errors,
      "DataFiles sheet must contain columns fileIdentifier and dataClass"
    )
    return(list(errors = errors, warnings = warnings))
  }

  unknownDataClass <- setdiff(
    unique(dataFiles$dataClass),
    unname(unlist(DATACLASS))
  )
  unknownDataClass <- unknownDataClass[!is.na(unknownDataClass)]
  if (length(unknownDataClass) > 0) {
    errors <- c(
      errors,
      paste(
        "Unknown DataClass values:",
        paste(unknownDataClass, collapse = ", ")
      )
    )
  }

  numericRows <- dataFiles[dataFiles$dataClass == DATACLASS[["numericValues"]]]
  if (nrow(numericRows) == 0) {
    return(list(errors = errors, warnings = warnings))
  }

  if (!("dictionary" %in% names(dataFiles))) {
    warnings <- c(
      warnings,
      "DataFiles has numericValues rows but no dictionary column"
    )
    return(list(errors = errors, warnings = warnings))
  }

  dictionaryValues <- unique(stats::na.omit(numericRows$dictionary))
  missingNumericSheets <- setdiff(dictionaryValues, wb$sheet_names)
  if (length(missingNumericSheets) > 0) {
    errors <- c(
      errors,
      paste(
        "Missing numeric values sheets:",
        paste(missingNumericSheets, collapse = ", ")
      )
    )
  }

  return(list(errors = errors, warnings = warnings))
}

#' Build AI Validation Suggestions
#'
#' @param errors Character vector with validation errors.
#' @param warnings Character vector with validation warnings.
#'
#' @return Character vector of suggestion messages.
#' @keywords internal
#' @noRd
.aiValidationSuggestions <- function(errors, warnings) {
  if (length(errors) == 0 && length(warnings) == 0) {
    return("Configuration looks ready for AI-assisted workflow generation")
  }

  return(c(
    "Run readObservedDataByDictionary for each dataClassType to confirm import behavior",
    "Ensure numericValues entries are unique and units are explicit"
  ))
}

#' Extract Unique Non-Missing Values from a Column
#'
#' @param dt A data.frame or data.table.
#' @param columnName Name of the column to extract values from.
#'
#' @return Character vector of unique non-missing values; empty if column is absent.
#' @keywords internal
#' @noRd
.safeUniqueValues <- function(dt, columnName) {
  if (!(columnName %in% names(dt))) {
    return(character())
  }

  return(unique(as.character(stats::na.omit(dt[[columnName]]))))
}

#' Truncate Value Vector to a Maximum Size
#'
#' @param x Vector of values.
#' @param maxItemsPerSection Maximum number of values to keep.
#'
#' @return Character vector truncated to `maxItemsPerSection` plus ellipsis when needed.
#' @keywords internal
#' @noRd
.truncateValues <- function(x, maxItemsPerSection) {
  x <- as.character(stats::na.omit(x))
  if (length(x) <= maxItemsPerSection) {
    return(x)
  }

  return(c(x[seq_len(maxItemsPerSection)], "..."))
}

#' Convert Values to Markdown Bullet Lines
#'
#' @param values Character vector values.
#'
#' @return Character vector of markdown bullet lines.
#' @keywords internal
#' @noRd
.toBulletLines <- function(values) {
  values <- as.character(values)
  if (length(values) == 0) {
    return("- none")
  }

  return(paste("-", values))
}
