# internal helpers --------------------------------------------------------

#' Convert a single xlsx sheet to the list structure used in JSON snapshots
#' @param filePath Path to the xlsx workbook.
#' @param sheetName Sheet name to read.
#' @return Named list with `column_names` and `rows`.
#' @keywords internal
#' @noRd
.rfExcelSheetToListStructure <- function(filePath, sheetName) {
  df <- xlsxReadData(
    wb = filePath,
    sheetName = sheetName,
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  sheetData <- list(column_names = names(df), rows = list())
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      sheetData$rows[[i]] <- as.list(sapply(
        df[i, , drop = FALSE],
        as.character
      ))
    }
  }
  return(sheetData)
}

#' Convert all sheets of an xlsx file to list structure
#' @param filePath Path to the xlsx workbook.
#' @return Named list of sheet list structures.
#' @keywords internal
#' @noRd
.rfExcelFileToListStructure <- function(filePath) {
  sheets <- readxl::excel_sheets(filePath)
  return(stats::setNames(
    lapply(sheets, function(s) .rfExcelSheetToListStructure(filePath, s)),
    sheets
  ))
}

#' Reconstruct a data frame from a JSON list structure
#' @param listStructure Named list with `column_names` and `rows`.
#' @return A `data.frame`.
#' @keywords internal
#' @noRd
.rfListStructureToDataFrame <- function(listStructure) {
  columnNames <- listStructure$column_names
  rows <- listStructure$rows
  df <- data.frame(
    matrix(ncol = length(columnNames), nrow = length(rows)),
    stringsAsFactors = FALSE
  )
  colnames(df) <- columnNames
  for (i in seq_along(rows)) {
    rowData <- rows[[i]]
    for (j in seq_along(columnNames)) {
      val <- rowData[[j]]
      df[i, j] <- if (is.null(val) || identical(val, "NA")) {
        NA_character_
      } else {
        as.character(val)
      }
    }
  }
  return(df)
}

#' Derive the JSON snapshot path for a given ProjectConfiguration.xlsx
#' @param xlsxPath Path to ProjectConfiguration.xlsx.
#' @param outputDir Optional output directory; defaults to same dir as xlsx.
#' @return Absolute path to the JSON snapshot file.
#' @keywords internal
#' @noRd
.rfSnapshotJsonPath <- function(xlsxPath, outputDir = NULL) {
  dir <- if (is.null(outputDir)) dirname(xlsxPath) else outputDir
  return(file.path(dir, sub("\\.xlsx$", ".json", basename(xlsxPath))))
}

#' Write all addon .xlsx files captured in a JSON snapshot back to disk.
#' @param addonsDf data.frame of the addons sheet (Property/Value columns).
#' @param configData Full JSON config list (contains per-addon sheet data).
#' @param outputDir Directory to write xlsx files into.
#' @keywords internal
#' @noRd
.rfRestoreAddonXlsxFiles <- function(addonsDf, configData, outputDir) {
  for (i in seq_len(nrow(addonsDf))) {
    prop <- addonsDf$Property[i]
    val <- addonsDf$Value[i]
    if (
      !is.na(val) &&
        grepl("\\.xlsx$", val, ignore.case = TRUE) &&
        prop %in% names(configData)
    ) {
      xlsxFilePath <- file.path(outputDir, val)
      if (!dir.exists(dirname(xlsxFilePath))) {
        dir.create(
          dirname(xlsxFilePath),
          recursive = TRUE,
          showWarnings = FALSE
        )
      }
      wbOut <- openxlsx::createWorkbook()
      for (sheetName in names(configData[[prop]])) {
        sheetDf <- .rfListStructureToDataFrame(configData[[prop]][[
          sheetName
        ]])
        openxlsx::addWorksheet(wbOut, sheetName)
        openxlsx::writeData(wbOut, sheet = sheetName, x = sheetDf)
      }
      openxlsx::saveWorkbook(wbOut, xlsxFilePath, overwrite = TRUE)
    }
  }
  return(invisible(NULL))
}

# public functions --------------------------------------------------------

#' Snapshot a ReportingFramework project configuration to JSON
#'
#' @description Extends \code{esqlabsR::snapshotProjectConfiguration()} with
#'   reporting-framework-specific metadata: the `ospsuiteReportingFrameworkVersion`
#'   and a list of RF-specific add-on file paths.
#'
#'   Note: overlapping file properties are excluded to avoid
#'   conflicts when the JSON is later restored via esqlabsR or RF restore functions.
#'
#' @param projectConfig A \code{ProjectConfigurationRF} object.
#' @param outputDir Directory where the JSON file is written.
#'   Defaults to the same directory as \code{ProjectConfiguration.xlsx}.
#' @param ... Additional arguments forwarded to
#'   \code{esqlabsR::snapshotProjectConfiguration()}.
#'
#' @return Invisible named list with the full snapshot data (including base esqlabsR content and RF-specific metadata).
#' @export
#' @family project initialization
snapshotProjectConfigurationRF <- function(
  projectConfig,
  outputDir = NULL,
  ...
) {
  checkmate::assertClass(projectConfig, "ProjectConfigurationRF")

  xlsxPath <- projectConfig$projectConfigurationFilePath
  checkmate::assertFileExists(xlsxPath)

  jsonPath <- .rfSnapshotJsonPath(xlsxPath, outputDir)
  effectiveOutputDir <- dirname(jsonPath)
  if (!dir.exists(effectiveOutputDir)) {
    dir.create(effectiveOutputDir, recursive = TRUE, showWarnings = FALSE)
  }

  # base snapshot (standard esqlabsR content + sheet 1 config data)
  configData <- esqlabsR::snapshotProjectConfiguration(
    projectConfig = projectConfig$baseProjectconfiguration,
    outputDir = effectiveOutputDir,
    ...
  )

  # addons sheet in the same {column_names, rows} format as all other esqlabsR sheets
  configData$projectConfigurationAddons <- .rfExcelSheetToListStructure(
    xlsxPath,
    "addons"
  )

  # include all .xlsx files referenced in addons rows
  configurationsFolder <- projectConfig$baseProjectconfiguration$configurationsFolder
  for (row in configData$projectConfigurationAddons$rows) {
    prop <- row[["Property"]]
    val <- row[["Value"]]
    if (
      !is.null(val) &&
        !is.na(val) &&
        grepl("\\.xlsx$", val, ignore.case = TRUE)
    ) {
      absPath <- fs::path_abs(val, start = configurationsFolder)
      if (file.exists(absPath)) {
        configData[[prop]] <- .rfExcelFileToListStructure(absPath)
      }
    }
  }

  jsonData <- jsonlite::toJSON(
    configData,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )
  writeLines(jsonData, jsonPath)

  return(invisible(configData))
}

#' Restore a ReportingFramework project configuration from a JSON snapshot
#'
#' @description Extends \code{esqlabsR::restoreProjectConfiguration()} by also
#'   restoring RF-specific metadata stored in the snapshot (e.g.
#'   `ospsuiteReportingFrameworkVersion`, add-on file paths).
#'
#' @param jsonPath Path to the \code{ProjectConfiguration.json} snapshot file.
#' @param outputDir Directory where xlsx files are restored.
#'   Defaults to the same directory as the JSON file.
#' @param ... Additional arguments forwarded to
#'   \code{esqlabsR::restoreProjectConfiguration()}.
#'
#' @return A \code{ProjectConfigurationRF} object for the restored project.
#' @export
#' @family project initialization
restoreProjectConfigurationRF <- function(jsonPath, outputDir = NULL, ...) {
  checkmate::assertFileExists(jsonPath)

  configData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)
  effectiveOutputDir <- if (is.null(outputDir)) {
    dirname(jsonPath)
  } else {
    outputDir
  }

  # base restore — recreates ProjectConfiguration.xlsx and standard xlsx files
  esqlabsR::restoreProjectConfiguration(
    jsonPath = jsonPath,
    outputDir = effectiveOutputDir,
    ...
  )

  xlsxPath <- file.path(
    effectiveOutputDir,
    sub("\\.json$", ".xlsx", basename(jsonPath))
  )

  if ("projectConfigurationAddons" %in% names(configData)) {
    # reconstruct addons sheet from {column_names, rows} structure
    addonsDf <- .rfListStructureToDataFrame(
      configData$projectConfigurationAddons
    )
    wb <- openxlsx::loadWorkbook(xlsxPath)
    if (!("addons" %in% wb$sheet_names)) {
      openxlsx::addWorksheet(wb, "addons")
    }
    openxlsx::writeData(wb, sheet = "addons", x = addonsDf)
    openxlsx::saveWorkbook(wb, xlsxPath, overwrite = TRUE)

    .rfRestoreAddonXlsxFiles(addonsDf, configData, effectiveOutputDir)
  }

  pcRF <- createProjectConfiguration(
    # nolint: object_usage_linter.
    path = xlsxPath,
    ignoreVersionCheck = TRUE
  )
  return(invisible(pcRF))
}
