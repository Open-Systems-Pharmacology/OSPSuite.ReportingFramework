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
#' @return A named list with workflow context information.
#' @export
#' @family project initialization
exportWorkflowContext <- function(
    projectConfiguration,
    file = "workflow-context.json",
    includeNumericValues = TRUE
) {
    checkmate::assertFlag(includeNumericValues)

    dataImporterConfigurationFile <- projectConfiguration$dataImporterConfigurationFile
    scenariosFile <- projectConfiguration$scenariosFile
    reportsFile <- projectConfiguration$addOns$reportsFile

    checkmate::assertFileExists(dataImporterConfigurationFile)
    checkmate::assertFileExists(scenariosFile)
    checkmate::assertFileExists(reportsFile)

    dataImportWb <- openxlsx::loadWorkbook(dataImporterConfigurationFile)
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
            dataImporterConfigurationFile = dataImporterConfigurationFile,
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
#' @return A character string containing the prompt.
#' @export
#' @family project initialization
createAIWorkflowPrompt <- function(
    projectConfiguration,
    task = "create-workflow",
    includeExamples = TRUE,
    maxItemsPerSection = 30
) {
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
            "- Data importer configuration:",
            context$configurationFiles$dataImporterConfigurationFile
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
        numericPreview <- head(numericPreview, maxItemsPerSection)

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
#' @return A character string containing the prompt.
#' @export
#' @family project initialization
createAIPromptFromTask <- function(
    projectConfiguration,
    task = c("create-workflow", "import-data", "build-plots", "debug-run"),
    includeExamples = TRUE,
    maxItemsPerSection = 30
) {
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
#' @return A list with `ok`, `errors`, `warnings`, and `suggestions`.
#' @export
#' @family project initialization
validateWorkflowForAI <- function(projectConfiguration, strict = FALSE) {
    checkmate::assertFlag(strict)

    errors <- character()
    warnings <- character()
    suggestions <- character()

    dataImporterConfigurationFile <- projectConfiguration$dataImporterConfigurationFile
    scenariosFile <- projectConfiguration$scenariosFile
    reportsFile <- projectConfiguration$addOns$reportsFile

    for (p in c(dataImporterConfigurationFile, scenariosFile, reportsFile)) {
        if (!file.exists(p)) {
            errors <- c(errors, paste("Missing configuration file:", p))
        }
    }

    if (length(errors) == 0) {
        wb <- openxlsx::loadWorkbook(dataImporterConfigurationFile)
        requiredSheets <- c("DataFiles")
        missingSheets <- setdiff(requiredSheets, wb$sheet_names)
        if (length(missingSheets) > 0) {
            errors <- c(
                errors,
                paste(
                    "Missing required sheets in DataImportConfiguration:",
                    paste(missingSheets, collapse = ", ")
                )
            )
        }

        if ("DataFiles" %in% wb$sheet_names) {
            dataFiles <- xlsxReadData(
                wb = wb,
                sheetName = "DataFiles",
                skipDescriptionRow = TRUE
            )

            if (!all(c("fileIdentifier", "dataClass") %in% names(dataFiles))) {
                errors <- c(
                    errors,
                    "DataFiles sheet must contain columns fileIdentifier and dataClass"
                )
            } else {
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
            }

            numericRows <- dataFiles[
                dataFiles$dataClass == DATACLASS[["numericValues"]]
            ]
            if (nrow(numericRows) > 0) {
                if (!("dictionary" %in% names(dataFiles))) {
                    warnings <- c(
                        warnings,
                        "DataFiles has numericValues rows but no dictionary column"
                    )
                } else {
                    dictionaryValues <- unique(stats::na.omit(
                        numericRows$dictionary
                    ))
                    missingNumericSheets <- setdiff(
                        dictionaryValues,
                        wb$sheet_names
                    )
                    if (length(missingNumericSheets) > 0) {
                        errors <- c(
                            errors,
                            paste(
                                "Missing numeric values sheets:",
                                paste(missingNumericSheets, collapse = ", ")
                            )
                        )
                    }
                }
            }
        }
    }

    if (length(errors) == 0 && length(warnings) == 0) {
        suggestions <- c(
            suggestions,
            "Configuration looks ready for AI-assisted workflow generation"
        )
    } else {
        suggestions <- c(
            suggestions,
            "Run readObservedDataByDictionary for each dataClassType to confirm import behavior",
            "Ensure numericValues entries are unique and units are explicit"
        )
    }

    ok <- length(errors) == 0 && (!strict || length(warnings) == 0)

    return(list(
        ok = ok,
        errors = unique(errors),
        warnings = unique(warnings),
        suggestions = unique(suggestions)
    ))
}

.safeUniqueValues <- function(dt, columnName) {
    if (!(columnName %in% names(dt))) {
        return(character())
    }

    return(unique(as.character(stats::na.omit(dt[[columnName]]))))
}

.truncateValues <- function(x, maxItemsPerSection) {
    x <- as.character(stats::na.omit(x))
    if (length(x) <= maxItemsPerSection) {
        return(x)
    }

    c(x[seq_len(maxItemsPerSection)], "...")
}

.toBulletLines <- function(values) {
    values <- as.character(values)
    if (length(values) == 0) {
        return("- none")
    }

    paste("-", values)
}
