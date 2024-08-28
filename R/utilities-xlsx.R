#' adds a new sheet to a workbook
#'
#' wrapps `openxlsx::addWorksheet`, and adds new data to the sheet
#'
#' @inheritParams openxlsx::addWorksheet
#' @param dt `data.table`  to attach
#'
#' @export
xlsxAddSheet <- function(wb, sheetName, dt) {
  if (sheetName %in% wb$sheet_names) {
    warning(paste(sheetName, "already exist"))
    # clear sheet by reading
    invisible(xlsxReadData(wb, sheetName))
  } else {
    openxlsx::addWorksheet(wb = wb, sheetName = sheetName)
  }
  openxlsx::writeData(wb = wb, sheet = sheetName, x = dt)

  return(invisible())
}

#' adds data to worksheet
#'
#' wrapps `openxlsx::writeData`, but delete before all current content
#'
#' @inheritParams openxlsx::writeData
#' @param dt `data.table` to write
#'
#' @export
xlsxWriteData <- function(wb, sheetName, dt) {
  # clear existing
  tmp <- xlsxReadData(wb, sheetName = sheetName)
  tmp[, names(tmp) := NA]
  openxlsx::writeData(wb = wb, sheet = sheetName, x = tmp)

  openxlsx::writeData(wb = wb, sheet = sheetName, x = dt)

  return(invisible())
}

#' create a clone of a sheet and set new contet
#'
#' wrapps openxlsx::cloneWorksheet but set new content
#'
#' @inheritParams openxlsx::cloneWorksheet
#' @param dt `data.table` with new content
#'
#' @return workbook with cloned sheet
xlsxCloneAndSet <- function(wb, clonedSheet, sheetName, dt) {
  if (!(sheetName %in% wb$sheet_names)) {
    openxlsx::cloneWorksheet(wb = wb, clonedSheet = clonedSheet, sheetName = sheetName)
  }

  # clear data by reading
  invisible(xlsxReadData(wb = wb, sheetName = sheetName))

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)

  return(wb)
}



#' reads data
#'
#' wrapps `openxlsx::read.xlsx` but returns data as data.table()
#'
#'
#' @param wb workbook or xlsxfile
#' @param sheetName sheet name
#' @param skipDescriptionRow `boolean`if TRUE first line is interpreted as a description and skipped
#' @param alwaysCharacter vector with column names which should be returned as character (typically identifier)
#'
#' @return `data.table` with sheet data
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
                         )) {
  dt <- data.table::setDT(openxlsx::read.xlsx(
    xlsxFile = wb,
    sheet = sheetName,
    sep.names = " ",
    skipEmptyRows = TRUE
  ))

  if (skipDescriptionRow) {
    dt <- dt[-1, ]
  }

  # Convert columns to character or numeric
  alwaysCharacter <- intersect(names(dt), alwaysCharacter)
  if (length(alwaysCharacter) > 0) {
    dt[, (alwaysCharacter) := lapply(.SD, as.character), .SDcols = alwaysCharacter]
  }

  convertibleCols <-
    suppressWarnings(names(dt)[sapply(dt, function(x) {
      all(is.na(x) | !is.na(as.numeric(x)))
    })])
  numericCols <- setdiff(convertibleCols, alwaysCharacter)
  if (length(numericCols) > 0) {
    dt[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
  }

  characterCols <- setdiff(names(dt), numericCols)
  if (length(characterCols) > 0) {
    dt[, (characterCols) := lapply(.SD, trimws), .SDcols = characterCols]
  }

  dt[dt == ""] <- NA

  return(dt)
}

# auxiliaries -------

#' Split the elements of a vector by comma
#'
#' This function takes an original vector as input and splits its elements by comma.
#'
#' @param originalVector The original vector to be split
#' @return A vector containing the split elements
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


# load default tables -------

#' load the properties for data groups
#'
#' @template projectConfig
#'
#' @return `data.table` with datagroupIds
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

#' load the scenario definitions
#'
#' @template projectConfig
#'
#' @return `data.table` with scenario definitions
#' @export
getScenarioDefinitions <- function(projectConfiguration) {
  return(xlsxReadData(
    wb = projectConfiguration$scenarioDefinitionFile,
    sheetName = "Scenarios",
    skipDescriptionRow = FALSE
  ))
}

#' load the output configurations
#'
#' @template projectConfig
#'
#' @return `data.table` with output configurations
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
