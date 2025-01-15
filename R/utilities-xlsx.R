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
                            stop(paste('ambigiuos header names in', sheetName, paste(names(existingData)[ix],sep = ',')))
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
#' @return An invisible NULL value.
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
#' @param skipDescriptionRow A logical value or an integer indicating whether the first row
#'     (or rows, if an integer) should be treated as description rows and skipped during reading.
#'      When set to TRUE, the first row is skipped; when set to a positive integer, that number of rows will be skipped.
#'     The 'Comment' column is also excluded from the resulting data table.
#' @param alwaysCharacter A character vector with column names or regex patterns that should be returned as character (typically identifiers).
#' @param emptyAsNA A logical value. If TRUE, empty strings are converted to NA.
#' @param convertHeaders A logical value. If TRUE, column names are converted to start with a lowercase letter.
#'
#' @return A `data.table` containing the sheet data.
#' @export
xlsxReadData <- function(wb, sheetName,
                         skipDescriptionRow = FALSE,
                         alwaysCharacter = c("Group", "Id$", "Ids$"),
                         emptyAsNA = TRUE,
                         convertHeaders = TRUE) {
  if (!any(class(wb) %in% "Workbook")) checkmate::assertFileExists(wb)

  # Read data from the specified sheet
  dt <- data.table::setDT(openxlsx::read.xlsx(
    xlsxFile = wb,
    sheet = sheetName,
    sep.names = " ",
    skipEmptyRows = TRUE,
  ))

  if (as.logical(skipDescriptionRow)) {
    dt <- dt[-seq_len(as.integer(skipDescriptionRow)) ]
    dt <- dt %>% dplyr::select(!any_of('Comment'))
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
    dt <- setHeadersToLowerCase(dt)

  return(dt)
}

#' Add new data to a config file using a template sheet
#'
#' If the template does not exist in the configuration file in the project directory,
#' it is taken from the configuration file of the package installation.
#' In this case, formats are not preserved.
#'
#' @param wb Current configuration file.
#' @param templateSheet Name of the template sheet.
#' @param sheetName Name of the new sheet.
#' @param dtNewData A `data.table` with new data.
#' @param templateXlsx A character string specifying the template xlsx file name (default is "Plots.xlsx").
#'
#' @return The current configuration file with the added sheet.
#' @export
addDataAsTemplateToXlsx <- function(wb, templateSheet, sheetName, dtNewData,templateXlsx = "Plots.xlsx") {
  # get template
  if (templateSheet %in% wb$sheet_names) {
    templateConfiguration <- xlsxReadData(wb = wb, sheetName = templateSheet)
  } else {
    templateConfiguration <-
      xlsxReadData(
        wb = system.file(
          "templates",
          templateXlsx,
          package = "ospsuite.reportingframework",
          mustWork = TRUE
        ),
        sheetName = templateSheet
      )
  }

  dtNewData <- rbind(templateConfiguration[1, ],
                     dtNewData, # nolint indentation_linter
                       fill = TRUE
  )

  if (templateSheet != sheetName) {
    xlsxCloneAndSet(wb = wb, clonedSheet = templateSheet, sheetName = sheetName, dt = dtNewData)
  } else {
    xlsxWriteData(wb = wb, sheetName = sheetName, dt = dtNewData)
  }

  return(wb)
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

#' Convert Data Table Column Names to Lowercase
#'
#' This function takes a data.table and converts the first letter of all column names to lowercase.
#'
#' @param dt A data.table object whose column names need to be converted to lowercase.
#'
#' @return The modified data.table with updated column names.
setHeadersToLowerCase <- function(dt){
  data.table::setnames(dt, sapply(names(dt), function(x)
    paste0(tolower(substring(
      x, 1, 1
    )), substring(x, 2))))
  return(dt)
}


#' Separate and Trim Helper Function
#'
#' This function separates the rows of a specified column in a data.table,
#' trims whitespace from the resulting values, and renames the column to remove
#' the plural 's' at the end of the column name.
#'
#' @param data A data.table containing the column to be processed.
#' @param columnName A string representing the name of the column to separate.
#'
#' @return A data.table with the specified column separated into multiple rows,
#'         trimmed of whitespace, and renamed to remove the plural 's'.
#' @keywords internal
separateAndTrim <- function(data, columnName) {
  # Separate rows based on the specified column and trim whitespace
  separatedData <- data %>%
    tidyr::separate_rows(!!sym(columnName), sep = ",") %>%
    data.table::setDT() %>%
    .[, (columnName) := trimws(get(columnName))] %>%
    data.table::setnames(old = columnName, new = sub("s$", "", columnName)) # Remove plural 's'

  return(separatedData)
}


# synchronize configuration tables ---------



#' Synchronize Scenarios Between Scenario and Plot Files
#'
#' This function synchronizes scenarios between two Excel files: one containing
#' scenarios for a project and the other containing scenarios related to plots.
#' It adds any missing scenarios from the scenarios file to the plots file.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing the file paths for scenariosFile and plotsFile.
#'
#' @return Returns invisibly.
#' @keywords internal
synchronizeScenariosWithPlots <- function(projectConfiguration) {

  # Load the workbooks for scenarios and plots
  wbSc <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  wbPl <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # Read data from the specified sheets
  scenariosSc <- xlsxReadData(wbSc, sheetName = 'Scenarios')
  scenariosPl <- xlsxReadData(wbPl, sheetName = 'Scenarios')

  # Identify scenarios that are in scenariosSc but not in scenariosPl
  scenariosToAdd <- setdiff(scenariosSc$scenario_name, scenariosPl$scenario)

  # If there are new scenarios to add, append them to scenariosPl
  if (length(scenariosToAdd) > 0) {
    # Create a data table for the new scenarios to add
    newScenarios <- data.table(
      scenario = scenariosToAdd,
      longName = gsub('_', ' ', scenariosToAdd),  # Replace underscores with spaces for long name
      shortName = gsub('_', ' ', scenariosToAdd)  # Replace underscores with spaces for short name
    )

    # Append new scenarios to the existing scenariosPl
    scenariosPl <- rbind(scenariosPl, newScenarios, fill = TRUE)

    # Write the updated scenarios back to the plots workbook
    xlsxWriteData(wb = wbPl, sheetName = 'Scenarios', scenariosPl)
    openxlsx::saveWorkbook(wb = wbPl,file = projectConfiguration$plotsFile,overwrite = TRUE)
  }

  return(invisible())
}

#' Synchronize Scenario Outputs with Plot Outputs
#'
#' This function synchronizes output paths between two Excel files: one containing
#' scenario outputs and the other containing plot outputs. It only writes back to the
#' Excel files if changes are detected in the output paths.
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing the file paths for scenariosFile and plotsFile.
#'
#' @return Returns invisibly.
#' @keywords internal
synchronizeScenariosOutputsWithPlots <- function(projectConfiguration) {

  # Load the workbooks for scenarios and plots
  wbSc <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  wbPl <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # Read data from the specified sheets
  outputsSc <- xlsxReadData(wbSc, sheetName = 'OutputPaths')
  outputsPl <- xlsxReadData(wbPl, sheetName = 'Outputs')

  # Merge the outputs based on the outputPathId
  opMerged <- merge(outputsSc, outputsPl[-1], by = 'outputPathId', all = TRUE, suffixes = c('.sc', '.pl'), sort = FALSE)

  # Initialize the outputPath column
  opMerged[, outputPath := '']

  # Check for inconsistencies between scenario and plot output paths
  if (nrow(opMerged[!is.na(outputPath.sc) & !is.na(outputPath.pl) & outputPath.sc != outputPath.pl]) > 0) {
    warning('Output definition in Scenario.xlsx and Plot.xlsx is inconsistent. Please synchronize manually')
  }

  # Synchronize output paths based on availability
  opMerged[!is.na(outputPath.sc) & !is.na(outputPath.pl) & outputPath.sc == outputPath.pl, outputPath := outputPath.pl]
  opMerged[!is.na(outputPath.sc) & is.na(outputPath.pl), outputPath := outputPath.sc]
  opMerged[is.na(outputPath.sc) & !is.na(outputPath.pl), outputPath := outputPath.pl]

  opMerged[,outputPath.sc:= NULL]
  opMerged[,outputPath.pl:= NULL]


  # Set column order for better readability
  data.table::setcolorder(opMerged, c('outputPathId', 'outputPath'))

  # Write back to Scenario workbook if changes are detected
  if (any(!(opMerged$outputPathId %in% outputsSc$outputPathId)) |
      any(!(opMerged[!is.na(outputPath)]$outputPath %in% outputsSc[!is.na(outputPath)]$outputPath))) {
    xlsxWriteData(wbSc, sheetName = 'OutputPaths', opMerged[, c('outputPathId', 'outputPath')])
    openxlsx::saveWorkbook(wb = wbSc,file = projectConfiguration$scenariosFile,overwrite = TRUE)
  }
  # Write back to Plot workbook if changes are detected
  if (any(!(opMerged$outputPathId %in% outputsPl$outputPathId)) |
      any(!(opMerged[!is.na(outputPath)]$outputPath %in% outputsPl[!is.na(outputPath)]$outputPath))) {
    xlsxWriteData(wbPl, sheetName = 'Outputs', rbind(outputsPl[1],opMerged))
    openxlsx::saveWorkbook(wb = wbPl,file = projectConfiguration$plotsFile,overwrite = TRUE)
  }

  return(invisible())
}





