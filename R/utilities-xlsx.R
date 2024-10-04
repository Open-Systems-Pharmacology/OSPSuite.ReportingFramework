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

  # make a copy otherwise data.table outside may be changed
  dt <- data.table::copy(dt)

  if (!(sheetName %in% wb$sheet_names)) {
    stop(paste("Sheet", sheetName, "does not exist."))
  }

  # Read existing data to determine dimensions
  existingData <- xlsxReadData(wb, sheetName = sheetName,convertHeaders = FALSE)

  # Check if existingData has more rows than dt
  if (nrow(existingData) > nrow(dt)) {
    # Add a data.table with NA values
    na_rows <- data.table(matrix(NA, nrow = nrow(existingData) - nrow(dt), ncol = ncol(dt)))
    data.table::setnames(na_rows, names(dt))  # Set the column names to match dt
    dt <- rbind(dt, na_rows)
  }

  # Convert data.table column names to match correct upper an lower case

  data.table::setnames(dt, old = names(dt),
                       new = unlist(lapply(names(dt), function(x){
                         ix = which(tolower(x) == tolower(names(existingData)))
                          if (length(ix) == 1){
                            newName = names(existingData)[ix]
                          } else if (length(ix) > 1) {
                            stop(paste('ambigiuos header ames in', sheetName, paste(names(existingData)[ix],sep = ',')))
                          } else {
                            newName = x
                          }
                         return(newName)
                         })))

  # Write new data
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
#' @export
xlsxCloneAndSet <- function(wb, clonedSheet, sheetName, dt) {
  if (!(sheetName %in% wb$sheet_names)) {
    openxlsx::cloneWorksheet(wb = wb, clonedSheet = clonedSheet, sheetName = sheetName)
  }

  # clear data by reading sheet
  invisible(xlsxReadData(wb, sheetName = sheetName))

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)

  return(invisible())
}

#' Read data from a worksheet
#'
#' This function wraps `openxlsx::read.xlsx` and returns the data as a `data.table`.
#'
#' @param wb A workbook object or a character string specifying the path to the xlsx file.
#' @param sheetName A character string specifying the name of the sheet to read.
#' @param skipDescriptionRow A logical value indicating if the first row should be interpreted as a description and skipped.
#' @param alwaysCharacter A character vector with column names or regex patterns that should be returned as character (typically identifiers).
#' @param emptyAsNA A logical value. If TRUE, empty strings are converted to NA.
#' @param convertHeaders A logical value. If TRUE, column names are converted to start with a lowercase letter.
#'
#' @return A `data.table` containing the sheet data.
#' @export
xlsxReadData <- function(wb, sheetName,
                         skipDescriptionRow = FALSE,
                         alwaysCharacter = c("group", "Id$", "Ids$"),
                         emptyAsNA = TRUE,
                         convertHeaders = TRUE) {
  if (!any(class(wb) %in% "Workbook")) checkmate::assertFileExists(wb)

  # Read data from the specified sheet
  dt <- data.table::setDT(openxlsx::read.xlsx(
    xlsxFile = wb,
    sheet = sheetName,
    sep.names = " ",
    skipEmptyRows = TRUE
  ))

  if (skipDescriptionRow) {
    dt <- dt[-1, ]
  }

  # Capture all columns matching the patterns in alwaysCharacter
  idColumns <- unlist(lapply(alwaysCharacter, function(pattern) {
    grep(pattern, names(dt), value = TRUE)
  }))
  alwaysCharacter <- unique(c(alwaysCharacter, idColumns))

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

  # Keeps NA and empty string consistent within one table
  if (emptyAsNA) {
    dt[dt == ""] <- NA
  } else {
    dt[is.na(dt)] <- ""
  }

  # Convert column names to start with a lowercase letter
  if (convertHeaders)
    data.table::setnames(dt, sapply(names(dt), function(x)
      paste0(tolower(substring(
        x, 1, 1
      )), substring(x, 2))))

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
  # avoid warning for global variable
  displayUnit <- NULL

  dtOutputPaths <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = "Outputs",
    skipDescriptionRow = TRUE
  )

  dtOutputPaths[, displayUnit := gsub("Âµ", "\u00B5", as.character(displayUnit))]
  dtOutputPaths[is.na(displayUnit), displayUnit := ""]

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
  # avoid warning for global variable
  captionText <- NULL

  dtTimeRange <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = "TimeRange",
    skipDescriptionRow = TRUE,
    emptyAsNA = FALSE
  )

  dtTimeRange$Tag <- factor(dtTimeRange$tag,
    levels = unique(dtTimeRange$tag),
    ordered = TRUE
  )

  return(dtTimeRange)
}
