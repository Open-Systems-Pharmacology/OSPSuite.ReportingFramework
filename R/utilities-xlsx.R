# openxlsx wrappings -----

#' Add a new sheet to a workbook with data
#'
#' This function wraps `openxlsx::addWorksheet` and adds data to the newly created sheet.
#' If the sheet already exists, it issues a warning and clears the existing content.
#'
#' @param wb A workbook object created by `openxlsx::loadWorkbook()`.
#' @param sheetName A character string specifying the name of the sheet to add.
#' @param dt A `data.table` containing the data to be written to the new sheet.
#'
#' @return An invisible NULL value.
#' @export
xlsxAddSheet <- function(wb, sheetName, dt) {
  if (sheetName %in% wb$sheet_names) {
    warning(paste(sheetName, "already exists. Existing content will be cleared."))
    invisible(xlsxReadData(wb, sheetName))
  } else {
    openxlsx::addWorksheet(wb = wb, sheetName = sheetName)
  }
  openxlsx::writeData(wb = wb, sheet = sheetName, x = dt)

  return(invisible())
}

#' Write data to a worksheet, clearing existing content
#'
#' This function wraps `openxlsx::writeData`, but deletes all current content before writing new data.
#'
#' @param wb A workbook object created by `openxlsx::loadWorkbook()`.
#' @param sheetName A character string specifying the name of the sheet where data will be written.
#' @param dt A `data.table` to write.
#'
#' @return An invisible NULL value.
#' @export
xlsxWriteData <- function(wb, sheetName, dt) {
  if (!(sheetName %in% wb$sheet_names)) {
    stop(paste("Sheet", sheetName, "does not exist."))
  }

  # Clear existing data
  tmp <- xlsxReadData(wb, sheetName = sheetName)
  tmp[, names(tmp) := NA]
  openxlsx::writeData(wb = wb, sheet = sheetName, x = tmp)

  openxlsx::writeData(wb = wb, sheet = sheetName, x = dt)

  return(invisible())
}

#' Clone a sheet and set new content
#'
#' This function wraps `openxlsx::cloneWorksheet` but sets new content in the cloned sheet.
#'
#' @param wb A workbook object created by `openxlsx::loadWorkbook()`.
#' @param clonedSheet A character string specifying the name of the sheet to clone.
#' @param sheetName A character string specifying the name of the new sheet.
#' @param dt A `data.table` with new content.
#'
#' @return A workbook object with the cloned sheet.
#' @export
xlsxCloneAndSet <- function(wb, clonedSheet, sheetName, dt) {
  if (!(sheetName %in% wb$sheet_names)) {
    openxlsx::cloneWorksheet(wb = wb, clonedSheet = clonedSheet, sheetName = sheetName)
  }

  # clear data by reading sheet
  invisible(xlsxReadData(wb, sheetName = sheetName))

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)

  return(wb)
}

#' Read data from a worksheet
#'
#' This function wraps `openxlsx::read.xlsx` but returns data as a `data.table`.
#'
#' @param wb A workbook or xlsx file.
#' @param sheetName A character string specifying the name of the sheet to read.
#' @param skipDescriptionRow A boolean indicating if the first line should be interpreted as a description and skipped.
#' @param alwaysCharacter A vector with column names that should be returned as character (typically identifiers).
#' @param emptyAsNA If TRUE empty strings are converted to NA
#'
#' @return A `data.table` containing the sheet data.
#' @export
xlsxReadData <- function(wb, sheetName,
                         skipDescriptionRow = FALSE,
                         alwaysCharacter = c(
                           "IndividualId",
                           "StudyId",
                           "group",
                           "outputPathId",
                           "DataGroupIds",
                           "DataGroupId"
                         ),
                         emptyAsNA = TRUE) {
  if (!any(class(wb) %in%  "Workbook")) checkmate::assertFileExists(wb)
  dt <- data.table::setDT(openxlsx::read.xlsx(
    xlsxFile = wb,
    sheet = sheetName,
    sep.names = " ",
    skipEmptyRows = TRUE
  ))

  if (skipDescriptionRow) {
    dt <- dt[-1, ]
  }

  # Convert specified columns to character
  alwaysCharacter <- intersect(names(dt), alwaysCharacter)
  if (length(alwaysCharacter) > 0) {
    dt[, (alwaysCharacter) := lapply(.SD, as.character), .SDcols = alwaysCharacter]
  }

  # Convert convertible columns to numeric
  convertibleCols <- suppressWarnings(names(dt)[sapply(dt, function(x) {
    all(is.na(x) | !is.na(as.numeric(x)))
  })])
  numericCols <- setdiff(convertibleCols, alwaysCharacter)
  if (length(numericCols) > 0) {
    dt[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
  }

  # Trim whitespace for character columns
  characterCols <- setdiff(names(dt), numericCols)
  if (length(characterCols) > 0) {
    dt[, (characterCols) := lapply(.SD, trimws), .SDcols = characterCols]
  }

  # keeps NA and empty string consistent with in one table
  if (emptyAsNA) {
    dt[dt == ""] <- NA
  } else {
    dt[is.na(dt)] <- ""
  }

  return(dt)
}
# auxiliary ---------

#' Split the elements of a vector by comma
#'
#' This function takes an original vector as input and splits its elements by comma.
#'
#' @param originalVector The original vector to be split.
#'
#' @return A vector containing the split elements.
#' @examples
#' originalVector <- c("group1, group2", "group3")
#' splitInputs(originalVector)
#' # Result: c('group1', 'group2', 'group3')
#' @export
splitInputs <- function(originalVector) {
  if (all(is.na(originalVector))) {
    return(NULL)
  }
  originalVector <- originalVector[!is.na(originalVector)]
  splitVector <- trimws(unlist(strsplit(originalVector, ",")))

  return(splitVector)
}

# get special tables ---------

#' Load the properties for data groups
#'
#' @template projectConfig
#'
#' @return A `data.table` with data group IDs.
#' @export
getDataGroups <- function(projectConfiguration) {
  dtDataGroups <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = "DataGroups",
    skipDescriptionRow = TRUE
  )

  data.table::setnames(dtDataGroups, old = "Group", new = "group")

  dtDataGroups$group <- factor(dtDataGroups$group,
                               levels = unique(dtDataGroups$group),
                               ordered = TRUE
  )

  return(dtDataGroups)
}

#' Load the scenario definitions
#'
#' @template projectConfig
#'
#' @return A `data.table` with scenario definitions.
#' @export
getScenarioDefinitions <- function(projectConfiguration) {
  return(xlsxReadData(
    wb = projectConfiguration$scenariosFile,
    sheetName = "Scenarios",
    skipDescriptionRow = FALSE
  ))
}

#' Load the output configurations
#'
#' @template projectConfig
#'
#' @return A `data.table` with output configurations.
#' @export
getOutputPathIds <- function(projectConfiguration) {
  dtOutputPaths <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = "Outputs",
    skipDescriptionRow = TRUE
  )

  data.table::setnames(dtOutputPaths, old = "OutputPathId", new = "outputPathId")

  dtOutputPaths[, DisplayUnit := gsub("Âµ", "\u00B5", as.character(DisplayUnit))]
  dtOutputPaths[is.na(DisplayUnit), DisplayUnit := ""]

  dtOutputPaths$outputPathId <- factor(dtOutputPaths$outputPathId,
                                       levels = unique(dtOutputPaths$outputPathId),
                                       ordered = TRUE
  )

  return(dtOutputPaths)
}

#' Load the time range tags
#'
#' @template projectConfig
#'
#' @return A `data.table` with time range tags.
#' @export
getTimeRangeTags <- function(projectConfiguration) {
  dtTimeRange <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = "TimeRange",
    skipDescriptionRow = TRUE
  )

  dtTimeRange[is.na(CaptionText), CaptionText := ""]

  dtTimeRange$Tag <- factor(dtTimeRange$Tag,
                            levels = unique(dtTimeRange$Tag),
                            ordered = TRUE
  )

  return(dtTimeRange)
}
