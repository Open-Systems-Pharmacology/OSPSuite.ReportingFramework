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
#' @return An invisible NULL value. The function performs an action (adding a sheet)
#'
#' @examples
#' \dontrun{
#' library(openxlsx)
#' wb <- loadWorkbook("example.xlsx")
#' data <- data.table(Name = c("Alice", "Bob"), Age = c(30, 25))
#' xlsxAddSheet(wb, "NewSheet", data)
#' }
#' @export
#' @family function to read from and write to xlsx
xlsxAddSheet <- function(wb, sheetName, dt) {
  # Input validation
  checkmate::assertClass(wb, "Workbook", null.ok = FALSE)
  checkmate::assertCharacter(sheetName, len = 1)
  checkmate::assertDataTable(dt,null.ok = FALSE)

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
#' @return An invisible NULL value. The function performs an action (adding data to a sheet)
#'
#' @examples
#' \dontrun{
#' wb <- loadWorkbook("example.xlsx")
#' data <- data.table(Name = c("Charlie", "Dana"), Age = c(28, 32))
#' xlsxWriteData(wb, "ExistingSheet", data)
#'}
#'
#' @export
#' @family function to read from and write to xlsx
xlsxWriteData <- function(wb, sheetName, dt) {
  # Input validation
  checkmate::assertClass(wb, "Workbook", null.ok = FALSE)
  checkmate::assertCharacter(sheetName, len = 1)
  checkmate::assertDataFrame(dt)

  # Make a copy to avoid modifying the original data.table
  dt <- data.table::copy(dt)

  # Check if the sheet exists
  checkSheetExists(wb, sheetName)

  # Read existing data to determine dimensions
  existingData <- xlsxReadData(wb, sheetName = sheetName, convertHeaders = FALSE)

  # Adjust data.table to match existing dimensions
  dt <- adjustDataTableDimensions(dt, existingData)

  # Align column names
  dt <- alignColumnNames(dt, existingData)

  # Write new data to the specified sheet
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
#' @return An invisible NULL value. The function performs an action (cole a sheet)
#'
#' @examples
#' \dontrun{
#' wb <- loadWorkbook("example.xlsx")
#' data <- data.table(Name = c("Eve", "Frank"), Age = c(22, 35))
#' xlsxCloneAndSet(wb, "ExistingSheet", "ClonedSheet", data)
#' }
#'
#' @export
#' @family function to read from and write to xlsx
xlsxCloneAndSet <- function(wb, clonedSheet, sheetName, dt) {
  # Input validation
  checkmate::assertClass(wb, "Workbook", null.ok = FALSE)
  checkmate::assertCharacter(clonedSheet, len = 1)
  checkmate::assertCharacter(sheetName, len = 1)
  checkmate::assertDataTable(dt)

  # Check if the clonedSheet exists in the workbook
  if (!(clonedSheet %in% wb$sheet_names)) {
    stop(paste("Sheet", clonedSheet, "does not exist in the workbook."))
  }
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
#' @param emptyAsNA A logical value. If TRUE, empty strings in character columns are converted to NA.
#' If FALSE NA is returned as empty string. Numeric columns Return always NA
#' @param convertHeaders A logical value. If TRUE, column names are converted to start with a lowercase letter.
#'
#' @return A `data.table` containing the sheet data.
#'
#' @examples
#' \dontrun{
#' wb <- loadWorkbook("example.xlsx")
#' data <- xlsxReadData(wb, "DataSheet", skipDescriptionRow = 1)
#' print(data)
#' }
#'
#' @export
#' @family function to read from and write to xlsx
xlsxReadData <- function(wb, sheetName = 1,
                         skipDescriptionRow = FALSE,
                         alwaysCharacter = c("Group", "Id$", "Ids$"),
                         emptyAsNA = TRUE,
                         convertHeaders = TRUE) {
  # Input validation
  if (!any(class(wb) %in% "Workbook")) checkmate::assertFileExists(wb)
  checkmate::assertLogical(skipDescriptionRow, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(alwaysCharacter, null.ok = TRUE)
  checkmate::assertLogical(emptyAsNA, len = 1)
  checkmate::assertLogical(convertHeaders, len = 1)


  # Read data from the specified sheet
  dt <- readSheetData(wb, sheetName)

  # Process the data based on the provided parameters
  dt <- processSheetData(dt, skipDescriptionRow, alwaysCharacter, emptyAsNA, convertHeaders)

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
#'
#' @examples
#' wb <- loadWorkbook("config.xlsx")
#' newData <- data.table(Name = c("Gina", "Hank"), Age = c(29, 33))
#' xlsxAddDataUsingTemplate(wb, "TemplateSheet", "NewDataSheet", newData)
#'
#' @export
#' @family function to read from and write to xlsx
xlsxAddDataUsingTemplate <- function(wb, templateSheet, sheetName, dtNewData, templateXlsx = "Plots.xlsx") {
  # Input validation
  checkmate::assertClass(wb, "Workbook", null.ok = FALSE)
  checkmate::assertCharacter(templateSheet, len = 1)
  checkmate::assertCharacter(sheetName, len = 1)
  checkmate::assertDataTable(dtNewData)

  # get template
  if (templateSheet %in% wb$sheet_names) {
    templateConfiguration <- xlsxReadData(wb = wb, sheetName = templateSheet)
  } else {
    templatePath <- system.file("templates", templateXlsx,
                                package = "ospsuite.reportingframework", mustWork = FALSE)

    if (!file.exists(templatePath)) {
      stop(paste("Template file", templatePath, "does not exist."))
    }
    templateConfiguration <-
      xlsxReadData(
        wb = templatePath,
        sheetName = templateSheet
      )
    xlsxAddSheet(wb, templateSheet, templateConfiguration)
  }

  dtNewData <- rbind(templateConfiguration[1, ],
                     setHeadersToLowerCase(dtNewData),
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
#' Check if the specified sheet exists in the workbook
#'
#' This function checks if the given sheet name exists in the workbook.
#'
#' @param wb A workbook object created by `openxlsx::loadWorkbook()`.
#' @param sheetName A character string specifying the name of the sheet to check.
#'
#' @return NULL. If the sheet does not exist, an error is thrown.
#' @keywords internal
checkSheetExists <- function(wb, sheetName) {
  if (!(sheetName %in% wb$sheet_names)) {
    stop(paste("Sheet", sheetName, "does not exist."))
  }
}

#' Adjust the dimensions of a data.table to match existing data
#'
#' This function adjusts the dimensions of a new data.table to match the existing data.
#' If the existing data has more rows, it appends NA rows to the new data.
#'
#' @param dt A data.table containing the new data to be written.
#' @param existingData A data.table containing the existing data in the specified sheet.
#'
#' @return A data.table with adjusted dimensions.
#' @keywords internal
adjustDataTableDimensions <- function(dt, existingData) {
  if (nrow(existingData) > nrow(dt)) {
    # Add NA rows if existing data has more rows than the new data
    naRows <- data.table(matrix(NA, nrow = nrow(existingData) - nrow(dt), ncol = ncol(dt)))
    data.table::setnames(naRows, names(dt)) # Set the column names to match dt
    dt <- rbind(dt, naRows)
  }
  return(dt)
}

#' Align column names of a data.table with existing data
#'
#' This function aligns the column names of a new data.table with those of the existing data.
#' It ensures that the new data has the same column names as the existing data, handling
#' potential ambiguities.
#'
#' @param dt A data.table containing the new data to be written.
#' @param existingData A data.table containing the existing data in the specified sheet.
#'
#' @return A data.table with aligned column names.
#' @keywords internal
alignColumnNames <- function(dt, existingData) {
  data.table::setnames(dt,
                       old = names(dt),
                       new = unlist(lapply(names(dt), function(x) {
                         ix <- which(tolower(x) == tolower(names(existingData)))
                         if (length(ix) == 1) {
                           newName <- names(existingData)[ix]
                         } else if (length(ix) > 1) {
                           stop(paste(
                             "ambiguous header names in sheet", existingData,
                             paste(names(existingData)[ix], collapse = ",")
                           ))
                         } else {
                           newName <- x
                         }
                         return(newName)
                       }))
  )
  return(dt)
}
#' Read data from the specified sheet using openxlsx
#'
#' This helper function reads data from a specified sheet in a workbook.
#'
#' @param wb A workbook object created by `openxlsx::loadWorkbook()`.
#' @param sheetName A character string specifying the name of the sheet to read.
#'
#' @return A `data.table` containing the raw data from the sheet.
#' @keywords internal
readSheetData <- function(wb, sheetName) {
  dt <- data.table::setDT(openxlsx::read.xlsx(
    xlsxFile = wb,
    sheet = sheetName,
    sep.names = " ",
    skipEmptyRows = TRUE
  ))
  return(dt)
}

#' Process the data read from the sheet
#'
#' This helper function processes the data by skipping description rows, converting specified columns to character,
#' converting empty strings to NA, and adjusting column headers.
#'
#' @param dt A `data.table` containing the raw data.
#' @param skipDescriptionRow A logical value or an integer indicating whether to skip description rows.
#' @param alwaysCharacter A character vector with column names or regex patterns that should be returned as character.
#' @param emptyAsNA A logical value. If TRUE, empty strings in character columns are converted to NA.
#' If FALSE NA is returned as empty string. Numeric columns Return always NA
#' @param convertHeaders A logical value. If TRUE, column names are converted to start with a lowercase letter.
#'
#' @return A processed `data.table`.
#' @keywords internal
processSheetData <- function(dt, skipDescriptionRow, alwaysCharacter, emptyAsNA, convertHeaders) {
  if (as.logical(skipDescriptionRow)) {
    dt <- dt[-seq_len(as.integer(skipDescriptionRow))]
    if ("Comment" %in% names(dt))
      dt <- dt[, !("Comment"), with = FALSE]
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
  dt <- convertColumnsToNumeric(dt, alwaysCharacter)

  # Trim whitespace for character columns and replace curly quotes
  dt <- cleanCharacterColumns(dt,emptyAsNA)

  # Convert column names to start with a lowercase letter
  if (convertHeaders) {
    dt <- setHeadersToLowerCase(dt)
  }

  return(dt)
}

#' Convert convertible columns to numeric
#'
#' This helper function converts columns in a data.table to numeric if possible.
#'
#' @param dt A `data.table` to process.
#' @param alwaysCharacter A character vector of column names to remain as character.
#'
#' @return A `data.table` with numeric conversions applied where appropriate.
#' @keywords internal
convertColumnsToNumeric <- function(dt, alwaysCharacter) {
  convertibleCols <- suppressWarnings(names(dt)[sapply(dt, function(x) {
    xWithoutNA <- x[!is.na(x) & x!='']
    return(length(xWithoutNA) == 0 || !any(is.na(as.numeric(xWithoutNA))))
  })])
  numericCols <- setdiff(convertibleCols, alwaysCharacter)
  if (length(numericCols) > 0) {
    dt[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
  }
  return(dt)
}

#' Clean character columns by trimming whitespace and replacing curly quotes
#'
#' This helper function trims whitespace from character columns and replaces curly quotes with straight quotes.
#'
#' @param dt A `data.table` to process.
#' @param emptyAsNA A logical value. If TRUE, empty strings in character columns are converted to NA.
#' If FALSE NA is returned as empty string.
#'
#' @return A `data.table` with cleaned character columns.
#' @keywords internal
cleanCharacterColumns <- function(dt,emptyAsNA) {
  characterCols <- setdiff(names(dt), names(dt)[sapply(dt, is.numeric)])
  if (length(characterCols) > 0) {
    # Trim whitespace for character columns
    dt[, (characterCols) := lapply(.SD, trimws), .SDcols = characterCols]
    # Replace curly quotes with straight quotes
    dt[, (characterCols) := lapply(.SD, function(x) {
      if (is.character(x)) {
        x <- gsub("\u201C", "\"", x) # Replace left double quote
        x <- gsub("\u201D", "\"", x) # Replace right double quote
        x <- gsub("\u2018", "'", x) # Replace left single quote
        x <- gsub("\u2019", "'", x) # Replace right single quote
      }
      return(x)
    }), .SDcols = characterCols]
    # Keeps NA and empty string consistent within one table
      dt[, (characterCols) := lapply(.SD, function(x) {
        if (emptyAsNA) {
          x[x == ""] <- NA_character_
        } else {
          x[is.na(x)] <- ''
        }
        return(x)
      }), .SDcols = characterCols]
  }

  return(dt)
}


#' Split the elements of a vector by comma
#'
#' This function takes an original vector as input and splits its elements by comma.
#'
#' @param originalVector The original vector to be split.
#'
#' @return A vector containing the split elements.
#' @examples
#' \dontrun{
#' originalVector <- c("group1, group2", "group3")
#' splitInputs(originalVector)
#' # Result: c('group1', 'group2', 'group3')
#' }
#'
#' @export
#' @family function to read from and write to xlsx
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
#'
#' @examples
#' dt <- data.table(FirstName = c("John", "Jane"), LastName = c("Doe", "Smith"))
#' dtLower <- setHeadersToLowerCase(dt)
#' print(names(dtLower)) # Result: c("firstname", "lastname")
#'
#' @export
#' @family function to read from and write to xlsx
setHeadersToLowerCase <- function(dt) {
  checkmate::assertDataTable(dt)

  data.table::setnames(dt, sapply(names(dt), function(x) {
    paste0(tolower(substring(
      x, 1, 1
    )), substring(x, 2))
  }))
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
separateAndTrimColumn <- function(data, columnName) {
  # Create a copy of the data.table to avoid modifying the original
  separatedData <- copy(data)

  # Use tidyr to separate rows based on the specified column and trim whitespace
  separatedData <- separatedData %>%
    tidyr::separate_rows(!!sym(columnName), sep = ",") %>%
    dplyr::mutate(!!sym(columnName) := trimws(!!sym(columnName))) %>%
    data.table() # Convert back to data.table

  # Rename the column to remove plural 's'
  setnames(separatedData, old = columnName, new = sub("s$", "", columnName))

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
  scenariosSc <- xlsxReadData(wbSc, sheetName = "Scenarios")
  scenariosPl <- xlsxReadData(wbPl, sheetName = "Scenarios")

  # Identify scenarios that are in scenariosSc but not in scenariosPl
  scenariosToAdd <- setdiff(scenariosSc$scenario_name, scenariosPl$scenario)

  # If there are new scenarios to add, append them to scenariosPl
  if (length(scenariosToAdd) > 0) {
    # Create a data table for the new scenarios to add
    newScenarios <- data.table(
      scenario = scenariosToAdd,
      longName = gsub("_", " ", scenariosToAdd), # Replace underscores with spaces for long name
      shortName = gsub("_", " ", scenariosToAdd) # Replace underscores with spaces for short name
    )

    # Append new scenarios to the existing scenariosPl
    scenariosPl <- rbind(scenariosPl, newScenarios, fill = TRUE)

    # Write the updated scenarios back to the plots workbook
    xlsxWriteData(wb = wbPl, sheetName = "Scenarios", scenariosPl)
    openxlsx::saveWorkbook(wb = wbPl, file = projectConfiguration$plotsFile, overwrite = TRUE)
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
synchronizeScenariosOutputsWithPlots <- function(projectConfiguration,
                                                 direction = c("bothways", "scenarioToPlot", "plotToScenario")) {
  # initialize variable to avoid messages
  outputPath <- outputPathPl <- outputPathSc <- NULL

  direction <- match.arg(direction)

  # Load the workbooks for scenarios and plots
  wbSc <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  wbPl <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # Read data from the specified sheets
  outputsSc <- xlsxReadData(wbSc, sheetName = "OutputPaths")
  outputsPl <- xlsxReadData(wbPl, sheetName = "Outputs")

  # Merge the outputs based on the outputPathId
  opMerged <- merge(outputsSc, outputsPl[-1],
                    by = "outputPathId",
                    all.x = direction %in% c("bothways", "scenarioToPlot"),
                    all.y = direction %in% c("bothways", "plotToScenario"),
                    suffixes = c("Sc", "Pl"),
                    sort = FALSE
  )

  # Initialize the outputPath column
  opMerged[, outputPath := ""]

  # Check for inconsistencies between scenario and plot output paths
  if (nrow(opMerged[!is.na(outputPathSc) & !is.na(outputPathPl) & outputPathSc != outputPathPl]) > 0) {
    warning("Output definition in Scenario.xlsx and Plot.xlsx is inconsistent. Please synchronize manually")
  }

  # Synchronize output paths based on availability
  opMerged[!is.na(outputPathSc) & !is.na(outputPathPl) & outputPathSc == outputPathPl, outputPath := outputPathPl]
  opMerged[!is.na(outputPathSc) & is.na(outputPathPl), outputPath := outputPathSc]
  opMerged[is.na(outputPathSc) & !is.na(outputPathPl), outputPath := outputPathPl]

  opMerged[, outputPathSc := NULL]
  opMerged[, outputPathPl := NULL]


  # Set column order for better readability
  data.table::setcolorder(opMerged, c("outputPathId", "outputPath"))

  # Write back to Scenario workbook if changes are detected
  if (any(!(opMerged$outputPathId %in% outputsSc$outputPathId)) |
      any(!(opMerged[!is.na(outputPath)]$outputPath %in% outputsSc[!is.na(outputPath)]$outputPath)) |
      nrow(outputsSc) != nrow(opMerged)) {
    xlsxWriteData(wbSc, sheetName = "OutputPaths", opMerged[, c("outputPathId", "outputPath")])
    openxlsx::saveWorkbook(wb = wbSc, file = projectConfiguration$scenariosFile, overwrite = TRUE)
  }
  # Write back to Plot workbook if changes are detected
  if (any(!(opMerged$outputPathId %in% outputsPl$outputPathId)) |
      any(!(opMerged[!is.na(outputPath)]$outputPath %in% outputsPl[!is.na(outputPath)]$outputPath)) |
      nrow(outputsPl) != (nrow(opMerged) + 1)) {
    xlsxWriteData(wbPl, sheetName = "Outputs", rbind(outputsPl[1], opMerged))
    openxlsx::saveWorkbook(wb = wbPl, file = projectConfiguration$plotsFile, overwrite = TRUE)
  }

  return(invisible())
}
