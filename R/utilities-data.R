# Read data by dictionary ----------

#' Read data by dictionary
#'
#' This function reads and processes data based on the provided project configuration.
#'
#' @param projectConfiguration An object containing project configuration details, including the path to the data importer configuration file.
#' @param spreadData If TRUE, information derived from observed data, such as identifiers and biometrics, is spread to other tables.
#' @param dataClassType A character string indicating the type of data class to process. Options are "timeprofile" or "pkParameter".
#' @param fileIds A character vector with file identifiers to be selected, if NULL (default) all are selected.
#'
#' @return A `data.table` containing the processed data based on the dictionary. The structure includes relevant columns defined in the data dictionary.
#' @export
readObservedDataByDictionary <- function(projectConfiguration,
                                         spreadData = TRUE,
                                         dataClassType = c("timeprofile", "pkParameter"),
                                         fileIds = NULL) {
  # avoid warning for global variable
  individualId <- outputPathId <- dataType <- NULL

  dataClassType <- match.arg(dataClassType)


  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)
  dataList <- xlsxReadData(
    wb = wb,
    sheetName = "DataFiles",
    skipDescriptionRow = TRUE
  )
  checkmate::assertCharacter(dataList$dataClass, any.missing = FALSE)
  dataList <- switch(dataClassType,
    timeprofile = dataList[dataClass %in% grep("^tp", unlist(DATACLASS), value = TRUE)],
    pkParameter = dataList[dataClass %in% grep("^pk", unlist(DATACLASS), value = TRUE)]
  )
  if (nrow(dataList) == 0) stop(paste("no datafiles defined for", dataClassType))

  if (!is.null(fileIds)) {
    checkmate::assertNames(fileIds, subset.of = dataList$fileIdentifier)
    dataList <- dataList[fileIdentifier %in% filePathFilter]
  }

  checkmate::assertFileExists(fs::path_abs(
    start = projectConfiguration$projectConfigurationDirPath,
    path = dataList$dataFile
  ))
  checkmate::assertNames(dataList$dictionary, subset.of = wb$sheet_names)

  # Loop through selected data files
  dataDT <- data.table()
  dict <- list()
  for (d in split(dataList, seq_len(nrow(dataList)))) {
    tmpData <- data.table::fread(fs::path_abs(
      start = projectConfiguration$projectConfigurationDirPath,
      path = d$dataFile
    ))

    tmpdict <- readDataDictionary(
      dictionaryFile = projectConfiguration$dataImporterConfigurationFile,
      sheet = d$dictionary,
      data = tmpData,
      dataClass = d$dataClass
    )

    dataDT <- rbind(dataDT,
      convertDataByDictionary( # nolint indentation_linter
        data = tmpData,
        dataFilter = d$dataFilter,
        dict = tmpdict,
        dictionaryName = d$dictionary
      ) %>%
        dplyr::mutate(dataClass = d$dataClass),
      fill = TRUE
    )
    # Get unique dictionary for columnType
    tmpdict <-
      tmpdict %>%
      dplyr::select(c("targetColumn", "type")) %>%
      unique()

    dict <- utils::modifyList(
      dict,
      as.list(tmpdict$type) %>%
        stats::setNames(tmpdict$targetColumn)
    )
  }

  dataDT[, dataType := "observed"]

  dataDT <- setDataTypeAttributes(dataDT, dict)

  validateObservedData(dataDT = dataDT, dataClassType)

  # Spread data to other tables
  if (spreadData) {
    # Define a list of functions and their arguments
    functionCalls <- list(
      list(
        func = updateDataGroupId, args = list(projectConfiguration = projectConfiguration, dataDT = dataDT),
        functionCall = "updateDataGroupId(projectConfiguration = projectConfiguration, dataDT = dataObserved)"
      ),
      list(
        func = updateOutputPathId, args = list(projectConfiguration = projectConfiguration, dataDT = dataDT),
        functionCall = "updateOutputPathId(projectConfiguration = projectConfiguration, dataDT = dataObserved)"
      ),
      list(
        func = addBiometricsToConfig, args = list(dataDT = dataDT, projectConfiguration = projectConfiguration),
        functionCall = "addBiometricsToConfig(projectConfiguration = projectConfiguration, dataDT = dataObserved)"
      ),
      list(
        func = setupVirtualTwinPopConfig, args = list(projectConfiguration = projectConfiguration, dataObserved = dataDT),
        functionCall = "setupVirtualTwinPopConfig(projectConfiguration = projectConfiguration, dataObserved = dataObserved)"
      )
    )

    # Loop through each function call
    for (call in functionCalls) {
      tryCatch(
        {
          do.call(call$func, call$args)
        },
        error = function(err) {
          warning(paste(
            "Error during execution of", call$functionCall,
            "\nMessage:", conditionMessage(err),
            "\nAre all relevant xlsx files closed? Retry manually."
          ))
        }
      )
    }
  }

  # Logging
  writeToLog(type = "Info", "Observed Data:")
  writeTableToLog(dataDT[, .(
    "No of data points" = .N,
    "No of individuals" = dplyr::n_distinct(individualId),
    "No of outputs" = dplyr::n_distinct(outputPathId)
  ),
  by = c("group")
  ])

  return(dataDT)
}

#' Validate Observed Data
#'
#' This function checks the integrity and validity of observed data in a `data.table`.
#' It verifies the presence of required attributes, checks for duplicates, and ensures that
#' there are no missing or empty values in the relevant columns. Additionally, it checks
#' for ambiguities in the Y unit and validates error type columns.
#'
#' @param dataDT A `data.table` containing observed data with the following relevant columns:
#'   - `individualId`: Unique identifier for individuals.
#'   - `group`: Group identifier.
#'   - `outputPathId`: Identifier for output paths.
#'   - `xValues`: Values for the x-axis.
#'   - `yUnit`: Unit for the Y values.
#'   - `yErrorType`: Type of error for Y values (optional).
#'   - `yErrorValues`: Values representing errors (optional).
#'   - `yMin`, `yMax`: Minimum and maximum Y values (optional).
#'   - `lloq`: Lower limit of quantification (optional).
#'   - `nBelowLLOQ`: Count of values below the lower limit of quantification (optional).
#'
#' @details
#' The function performs several checks, including:
#' - Ensuring all data columns have the appropriate attributes.
#' - Verifying that the data is unique based on specified identifier columns.
#' - Checking for NA or empty values in all relevant columns.
#' - Ensuring that the Y unit is consistent across output paths.
#' - Validating the presence of necessary columns based on the Y error type.
#'
#' @return NULL This function performs checks and stops execution if any validation fails.
#' It does not return a value.
#'
#' @export
validateObservedData <- function(dataDT, dataClassType) {
  # Initialize variables used for data.tables
  yUnit <- NULL
  # Check column Identifier
  columnsWithAttributes <- lapply(dataDT, attr, "columnType")
  columnsWithAttributes <-
    names(columnsWithAttributes)[unlist(lapply(columnsWithAttributes, function(col) {
      !is.null(col)
    }))]
  if (!all(names(dataDT) %in% columnsWithAttributes)) {
    warning(
      paste0(
        'Some data columns have no attribute: "',
        paste(setdiff(names(dataDT), columnsWithAttributes), collapse = '", "'),
        '"'
      )
    )
  }

  # Check data validity
  colIdentifier <-
    intersect(
      c("individualId", "group", "outputPathId", "xValues", "pkParameter"),
      names(dataDT)
    )
  if (any(duplicated(dataDT, by = colIdentifier))) {
    stop(
      paste(
        "Data must be unique in columns",
        paste(colIdentifier, collapse = ", ")
      )
    )
  }
  for (col in setdiff(
    names(dataDT),
    c(
      "lloq", "yUnit",
      "yErrorValues", "yErrorType", "nBelowLLOQ",
      "unit", "errorValues", "errorType" # columns for pkParameter
    )
  )) {
    if (any(is.na(dataDT[[col]]) | dataDT[[col]] == "")) {
      warning(paste("Data contains NAs or empty values in column", col))
    }
  }


  validateDataUnit <- function(colIdentifier, colUnit) {
    unitCounts <- dataDT[, .(N = uniqueN(get(colUnit))), by = colIdentifier]
    ambiguousUnits <- unitCounts[N > 1]

    if (nrow(ambiguousUnits) > 0) {
      unitSummary <- dataDT[,
        .(units = paste("units:", gsub(
          "NA", "emptyString",
          paste(unique(get(colUnit)),
            collapse = ", "
          )
        ))),
        by = colIdentifier
      ]
      tmp <- merge(ambiguousUnits, unitSummary, by = colIdentifier)

      summaryString <- paste(apply(tmp[, !"N", with = FALSE], 1, function(x) {
        paste(x, collapse = " ")
      }), collapse = " | ")
      warning(paste(
        "Ambiguous units:", summaryString,
        "\nPlease check if this acceptable, e.g. pkParameter as ratio and absolute values."
      ))
    }
  }

  validateErrorType <- function(errorTypeCol, errorValuesCol, minCol, maxCol) {
    if (errorTypeCol %in% names(dataDT)) {
      if (any(dataDT$yErrorType %in% unlist(ospsuite::DataErrorType))) {
        checkmate::assertNames(
          names(dataDT),
          must.include = errorValuesCol
        )
      } else {
        checkmate::assertNames(
          names(dataDT),
          must.include = c(minCol, maxCol)
        )
      }
    }
  }

  if (dataClassType == "timeprofile") {
    validateDataUnit(colIdentifier = c("outputPathId"), colUnit = "yUnit")
    validateErrorType(errorTypeCol = "yErrorType", errorValuesCol = "yErrorValues", minCol = "yMin", maxCol = "yMax")
  } else if (dataClassType == "pkParameter") {
    validateDataUnit(colIdentifier = c("outputPathId", "pkParameter"), colUnit = "unit")
    validateErrorType(errorTypeCol = "errorType", errorValuesCol = "errorValues", minCol = "minValue", maxCol = "maxValue")
  }
}

#' Read data dictionary
#'
#' This function reads the data dictionary based on the provided file and sheet.
#'
#' @param dictionaryFile The file containing the data dictionary.
#' @param sheet The sheet within the data dictionary file.
#' @param data The data to be used with the dictionary.
#' @param dataClass Class of data, either "tp Individual" or "tp Aggregated".
#'
#' @return A `data.table` containing the data dictionary.
#' @keywords internal
readDataDictionary <- function(dictionaryFile,
                               sheet,
                               data,
                               dataClass) {
  # Initialize variables used for data.tables
  sourceColumn <- filter <- NULL

  dict <- xlsxReadData(wb = dictionaryFile, sheetName = sheet, skipDescriptionRow = TRUE)

  if (dataClass == DATACLASS$tpIndividual) {
    checkmate::assertNames(
      dict$targetColumn,
      must.include = c(
        "studyId",
        "individualId",
        "group",
        "outputPathId",
        "xValues",
        "yValues",
        "yUnit"
      ), .var.name = paste("Check for missing targetColumns in dictionary", sheet)
    )
  } else if (dataClass == DATACLASS$tpAggregated) {
    checkmate::assertNames(
      dict$targetColumn,
      must.include = c(
        "studyId",
        "group",
        "outputPathId",
        "xValues",
        "yValues",
        "yUnit",
        "yErrorType",
        "nBelowLLOQ"
      ), .var.name = paste("Check for missing targetColumns in dictionary", sheet)
    )
  } else if (dataClass == DATACLASS$pkIndividual) {
    checkmate::assertNames(
      dict$targetColumn,
      must.include = c(
        "studyId",
        "individualId",
        "group",
        "outputPathId",
        "values",
        "unit"
      ), .var.name = paste("Check for missing targetColumns in dictionary", sheet)
    )
  } else if (dataClass == DATACLASS$pkAggregated) {
    checkmate::assertNames(
      dict$targetColumn,
      must.include = c(
        "studyId",
        "group",
        "outputPathId",
        "values",
        "unit",
        "errorType",
        "numberOfIndividuals"
      ), .var.name = paste("Check for missing targetColumns in dictionary", sheet)
    )
  }

  tmp <- dict[is.na(sourceColumn) & is.na(filter), ]
  if (nrow(tmp) > 0) {
    stop(paste0(
      'Either sourceColumn or Filter on sourceColumn has to be filled in dictionary "', sheet,
      '" for targetColumn(s) "', paste(tmp$targetColumn, collapse = '", "'), '"'
    ))
  }

  checkmate::assertNames(
    x = dict[!is.na(sourceColumn)]$sourceColumn,
    subset.of = names(data),
    .var.name = paste("Source column of", sheet)
  )

  return(dict)
}

#' Convert data by dictionary
#'
#' This function converts the data based on the provided dictionary and filters.
#'
#' @param data The data to be converted.
#' @param dataFilter The filter to be applied to the data.
#' @param dict The dictionary to be used for conversion.
#' @param dictionaryName The name of the dictionary.
#' @return A `data.table` containing the converted data.
#' @keywords internal
convertDataByDictionary <- function(data,
                                    dataFilter,
                                    dict,
                                    dictionaryName) {
  # Initialize variables used for data.tables
  targetColumn <- sourceColumn <- xUnit <- filter <- type <- NULL

  # Execute data filter
  if (!is.na(dataFilter) & dataFilter != "") data <- data[eval(parse(text = dataFilter))]

  # Execute all filters
  if (any(!is.na(dict$filter))) {
    dictFilters <- dict[!is.na(filter)]

    for (myFilter in split(dictFilters, seq_len(nrow(dictFilters)))) {
      tryCatch(
        {
          data[
            eval(parse(text = myFilter$filter)),
            (myFilter$targetColumn) := eval(parse(text = myFilter$filterValue))
          ]
        },
        error = function(err) {
          warning(paste0(
            "tpDictionary: '", dictionaryName,
            "'; targetColumn: '", myFilter$targetColumn,
            "'; filter: '", myFilter$filter,
            "'; filterValue: '", myFilter$filterValue,
            "'"
          ))
          stop(conditionMessage(err))
        }
      )
    }
  }

  # Rename columns to target columns
  dictColumns <- dict[!is.na(sourceColumn) & sourceColumn != targetColumn]

  checkmate::assertCharacter(
    dictColumns$targetColumn,
    unique = TRUE,
    .var.name = paste("Target columns with source columns of", dictionaryName)
  )

  # Create new columns for duplicated old names; do not use setnames as source columns may not be unique
  for (iRow in seq_len(nrow(dictColumns))) {
    data[, (dictColumns$targetColumn[iRow]) := data[[dictColumns$sourceColumn[iRow]]]]
  }

  # Reduce to defined columns
  data <- data %>%
    dplyr::select(unique(dict$targetColumn))

  # Add time unit
  if ("xValues" %in% dict$targetColumn) {
    data[, xUnit := dict[targetColumn == "xValues"]$sourceUnit]
  }

  # Transfer identifier to character without commas
  data <- convertIdentifierColumns(
    dt = data,
    identifierCols = dict[type == "identifier"]$targetColumn
  )

  data <- convertBiometrics(data, dict)

  return(data)
}

#' Converts biometric columns to default unit
#'
#' @inheritParams convertDataByDictionary
#'
#' @return A `data.table` with converted columns.
#' @keywords internal
convertBiometrics <- function(data, dict, dictionaryName) {
  # Initialize variables used for data.tables
  targetColumn <- gender <- NULL

  for (col in intersect(names(BIOMETRICUNITS), dict$targetColumn)) {
    unitFactor <- ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(BIOMETRICUNITS[[col]]),
      values = 1,
      targetUnit = BIOMETRICUNITS[[col]],
      sourceUnit = dict[targetColumn == col]$sourceUnit[1]
    )

    data[, (col) := get(col) * unitFactor]
  }

  if ("gender" %in% dict$targetColumn) {
    data[, gender := ifelse(gender == 1, "MALE", gender)]
    data[, gender := ifelse(gender == 2, "FEMALE", gender)]
    data[, gender := ifelse(gender == "M", "MALE", gender)]
    data[, gender := ifelse(gender == "F", "FEMALE", gender)]
    data[, gender := toupper(gender)]

    data[, gender := ifelse(gender != "MALE" & gender != "FEMALE", "UNKNOWN", gender)]

    if (any(data$gender == "UNKNOWN")) {
      warning(paste("Unknown gender in data set"))
    }
  }

  return(data)
}

#' Update Data Group IDs in Project Configuration
#'
#' This function updates the Data Groups sheet in an Excel workbook based on a provided data table.
#' It reads existing data group IDs, merges them with new data, and ensures that the identifiers remain unique.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration`
#' containing configuration details
#' @param dataDT A `data.table` containing the new data to be added to the Data Groups sheet.
#' It must have columns that can be matched with existing identifiers, including "group", "studyId", and "studyArm".
#'
#' @details
#' The function loads the existing Data Groups from the specified Excel workbook, selects relevant columns from the input data,
#' and appends new entries while maintaining the uniqueness of the identifiers. The updated data is then written back to the workbook.
#'
#' @return NULL This function updates the Excel workbook in place and does not return a value. It is called for its side effects.
#'
#' @export
updateDataGroupId <- function(projectConfiguration, dataDT) {
  # Initialize variables used for data.tables
  studyId <- group <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dtDataGroupIds <- xlsxReadData(wb = wb, sheetName = "DataGroups")
  identifierCols <- intersect(c("group", "studyId", "studyArm"), names(dataDT))

  colsSelected <- unique(c(
    identifierCols,
    setdiff(
      getColumnsForColumnType(dt = dataDT, columnTypes = "metadata"),
      c("compartmnt", "molecule", "organ")
    )
  ))

  dtDataGroupIdsNew <- dataDT %>%
    dplyr::select(dplyr::all_of(colsSelected)) %>%
    unique() %>%
    dplyr::mutate(studyId = as.character(studyId)) %>%
    dplyr::mutate(group = as.character(group)) %>%
    dplyr::mutate(displayName = as.character(group))

  dtDataGroupIdsNew <-
    dtDataGroupIdsNew[!(group %in% dtDataGroupIds$group)]

  if (nrow(dtDataGroupIdsNew) > 0) {
    dtDataGroupIds <- rbind(dtDataGroupIds,
      dtDataGroupIdsNew, # nolint indentation_linter
      fill = TRUE
    )

    xlsxWriteData(wb = wb, sheetName = "DataGroups", dt = dtDataGroupIds)
    openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
  }

  return(invisible())
}

#' Update Output Path IDs in Project Configuration
#'
#' This function updates the Outputs sheet in an Excel workbook based on a provided data table,
#' ensuring that output path IDs remain unique.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration` containing configuration details.
#'
#' @param dataDT A `data.table` containing new output path IDs to be added to the Outputs sheet.
#' It must include a column named "outputPathId".
#'
#' @details
#' The function loads the existing Output Paths from the specified Excel workbook, extracts unique output path IDs from the input data,
#' and appends them while maintaining uniqueness. The updated data is then written back to the workbook.
#'
#' @return NULL This function updates the Excel workbook in place and does not return a value. It is called for its side effects.
#'
#' @export
updateOutputPathId <- function(projectConfiguration, dataDT) {
  # Initialize variables used for data.tables
  outputPathId <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  dtOutputPaths <- xlsxReadData(wb = wb, sheetName = "Outputs")

  dtOutputPathsNew <- dataDT[!(outputPathId %in% dtOutputPaths$outputPathId), c("outputPathId")] %>%
    unique() %>%
    dplyr::mutate(outputPathId = as.character(outputPathId))

  if (nrow(dtOutputPathsNew) > 0) {
    dtOutputPaths <- rbind(dtOutputPaths,
      dtOutputPathsNew, # nolint indentation_linter
      fill = TRUE
    )

    xlsxWriteData(wb = wb, sheetName = "Outputs", dt = dtOutputPaths)
    openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

    synchronizeScenariosWithPlots(projectConfiguration)
  }

  return(invisible())
}

#' Add biometrics information to config
#'
#' @param projectConfiguration Object of class `ProjectConfiguration` containing information on paths and file names
#' @param dataDT A `data.table` with observed data.
#' @param overwrite If TRUE, existing rows will be overwritten.
#' @export
addBiometricsToConfig <- function(projectConfiguration, dataDT, overwrite = FALSE) {
  if (!("individualId" %in% names(dataDT))) {
    return(invisible())
  }

  checkmate::assertFileExists(projectConfiguration$individualsFile)

  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

  dtIndividualBiometrics <- xlsxReadData(wb = wb, sheetName = "IndividualBiometrics")
  if (overwrite) {
    dtIndividualBiometrics <- dtIndividualBiometrics[1, ]
  }

  biometrics <-
    dataDT %>%
    dplyr::select(
      c(
        "individualId",
        names(dataDT)[unlist(lapply(dataDT, attr, "columnType")) == "biometrics"]
      )
    ) %>%
    unique()

  for (col in names(biometrics)) {
    newName <- grep(col, names(dtIndividualBiometrics), ignore.case = TRUE, value = TRUE)
    if (newName != "") {
      data.table::setnames(biometrics, old = col, new = newName)
    }
  }

  if (!("species" %in% names(biometrics))) biometrics[["species"]] <- ospsuite::Species$Human

  # Merge old and new tables
  dtIndividualBiometrics <-
    rbind(dtIndividualBiometrics,
      biometrics, # nolint indentation_linter
      fill = TRUE
    )

  # If overwrite FALSE take original located at the top, otherwise take new rows located at the bottom
  dtIndividualBiometrics <-
    dtIndividualBiometrics[!duplicated(dtIndividualBiometrics,
      by = "individualId",
      fromLast = overwrite
    )]
  xlsxWriteData(wb = wb, sheetName = "IndividualBiometrics", dt = dtIndividualBiometrics)

  openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

  return(invisible())
}

# Converts data.table with observed data to `DataCombined` object ----------

#' Converts data.table with observed data to `ospsuite::DataCombined` object
#'
#' The `data.table` must be formatted like a table produced by `readObservedDataByDictionary`.
#'
#' @param dataDT A `data.table` to convert.
#'
#' @return An object of class `DataCombined`.
#' @export
convertDataTableToDataCombined <- function(dataDT) {
  validateObservedData(dataDT = dataDT, dataClassType = "timeprofile")

  groupedData <- groupDataByIdentifier(dataDT = dataDT)

  dataCombined <- ospsuite::DataCombined$new()

  for (groupData in groupedData) {
    dataSet <- createDataSets(groupData)
    dataSet <- addMetaDataToDataSet(dataSet, groupData)

    dataCombined$addDataSets(
      dataSets = dataSet,
      groups = as.character(groupData$group[1])
    )
  }

  return(dataCombined)
}

#' Groups the data by identifier
#'
#' @param dataDT A `data.table` to be grouped.
#'
#' @return A list with grouped data.
#' @keywords internal
groupDataByIdentifier <- function(dataDT) {
  checkmate::assert_disjunct(names(dataDT), ".groupBy")
  .groupBy <- NULL

  # Group data by identifier
  groupBy <- getColumnsForColumnType(dt = dataDT, columnTypes = "identifier")

  # Use a copy to keep the data.table outside the function unchanged
  dataDT <- data.table::copy(dataDT) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::all_of(groupBy))) %>%
    dplyr::mutate(.groupBy = paste(!!!dplyr::syms(groupBy), sep = "_")) %>%
    dplyr::ungroup()

  groupedData <- dataDT %>%
    dplyr::group_split(.groupBy)

  # Create a named list with.groupBy as names
  names(groupedData) <- unlist(lapply(groupedData, function(x) {
    unique(x[[".groupBy"]])
  }))

  return(groupedData)
}

#' Function to create data sets from grouped data
#'
#' @param groupData A `data.table` unique for identifier.
#'
#' @return An object of class 'DataSet'.
#' @keywords internal
createDataSets <- function(groupData) {
  # Initialize variables used for data.tables
  lloq <- NULL

  groupData <- data.table::as.data.table(groupData)
  groupName <- groupData$.groupBy[1]
  dataSet <- ospsuite::DataSet$new(groupName)
  dataSet$setValues(
    xValues = groupData$xValues,
    yValues = groupData$yValues
  )

  if (dplyr::n_distinct(groupData$yUnit) > 1) {
    stop(paste(
      "DataDT to combinedData: y Unit for dataset",
      groupName, "is not unique"
    ))
  }
  dataSet$yDimension <- ospsuite::getDimensionForUnit(groupData$yUnit[1])
  dataSet$yUnit <- groupData$yUnit[1]

  if (dplyr::n_distinct(groupData$xUnit) > 1) {
    stop(paste(
      "DataDT to combinedData: x Unit for dataset",
      groupName, "is not unique"
    ))
  }
  dataSet$xUnit <- groupData$xUnit[1]

  if (any(!is.na(groupData$lloq))) {
    lLOQ <- groupData[!is.na(lloq)]$lloq
    if (dplyr::n_distinct(lLOQ) > 1) {
      warning(paste(
        "DataDT to combinedData: More than one LLOQ for dataset",
        groupName,
        "is set to minimal"
      ))
    }
    lLOQ <- min(lLOQ)
    dataSet$LLOQ <- lLOQ
  }

  return(dataSet)
}

#' Add meta data to a data set
#'
#' @param dataSet A `DataSet` object with observed data.
#' @param groupData Corresponding `data.table` with meta data.
#'
#' @return A `DataSet` with observed data with added metadata.
#' @keywords internal
addMetaDataToDataSet <- function(dataSet, groupData) {
  metaColumns <-
    getColumnsForColumnType(
      dt = groupData,
      columnTypes = c("covariate", "biometrics", "identifier")
    )
  metaData <- groupData %>%
    dplyr::select(dplyr::all_of(metaColumns)) %>%
    unique() %>%
    as.list()
  for (col in metaColumns) {
    dataSet$addMetaData(name = col, value = as.character(metaData[[col]]))
  }

  return(dataSet)
}

# Converts `DataCombined` object to data.table  ----------

#' Converts object of class `DataCombined` to data.table with attributes.
#'
#' Format corresponds to `data.table` produced by `readObservedDataByDictionary`.
#'
#' @param datacombined The input `DataCombined` object.
#'
#' @return A `data.table` containing the converted data.
#' @export
convertDataCombinedToDataTable <- function(datacombined) {
  # Initialize variables used for data.tables
  dataClass <- yErrorValues <- NULL

  dataDT <- datacombined$toDataFrame() %>%
    data.table::setDT()

  # Delete columns not needed
  dataDT <- dataDT[, which(colSums(is.na(dataDT)) != nrow(dataDT)), with = FALSE]

  # Set DataClass
  dataDT[, dataClass := ifelse(any(!is.na(yErrorValues)), DATACLASS$tpAggregated, DATACLASS$tpIndividual),
    by = "group"
  ] # nolint indentation_linter

  if (any(dataDT$dataClass == DATACLASS$tpIndividual) &&
    !("individualId" %in% names(dataDT))) { # nolint indentation_linter
    stop("IndividualData needs meta data individualId")
  }
}

# Data aggregation ------------

#' Aggregate Observed Data Groups
#'
#' This function aggregates observed data based on specified groups and an aggregation method.
#' It allows for different aggregation techniques, including geometric and arithmetic standard deviations,
#' percentiles, or a user-defined custom function.
#'
#' The function also checks for values below the Lower Limit of Quantification (LLOQ) and adjusts the
#' aggregated results accordingly. For aggregationFlag 'GeometricStdDev' and 'GeometricStdDev', the statistics at any time point will only be calculated
#' if at least 2/3 of the individual data were measured and were above the lower limit of quantification (lloq).
#' For aggregationFlag 'Percentile', the statistics will only be calculated if less than or equal to 1/2 of the data is above LLOQ.
#' If you use aggregationFlag 'Custom', please set parameters lloqCheckColumns2of3 and lloqCheckColumns1of2 accordingly.
#'
#' A custom function should take a numeric vector `y` as input and return a list containing:
#' - `yValues`: The aggregated value (e.g., mean).
#' - `yMin`: The lower value of the aggregated data (e.g., mean - sd).
#' - `yMax`: The upper value of the aggregated data (e.g., mean + sd).
#' - `yErrorType`: A string indicating the type of error associated with the aggregation,
#' it is used in plot legends and captions.
#' It must be a concatenation of the descriptor of yValues and the descriptor of yMin - yMax range
#' separated by "|" (e.g., "mean | standard deviation" or "median | 5th - 95th percentile").
#'
#' @param dataObserved A `data.table` containing observed data.
#' @param groups A character vector specifying the groups to aggregate.
#' If NULL, all available groups are used.
#' @param aggregationFlag A character string indicating the aggregation method.
#' Options include "GeometricStdDev", "ArithmeticStdDev", "Percentiles", or "Custom".
#' @param percentiles A numeric vector of percentiles to calculate if aggregationFlag is "Percentiles".
#' Default is c(5, 50, 95).
#' @param groupSuffix A character string to append to group names in the aggregated output.
#' Default is 'aggregated'.
#' @param customFunction A custom function for aggregation if aggregationFlag is "Custom".
#' Default is NULL.
#' @param lloqCheckColumns2of3 A character vector specifying columns to check for LLOQ (Lower Limit of Quantification) for 1/3 data points.
#' Default is NULL, is used only for aggregationFlag "Custom".
#' @param lloqCheckColumns1of2 A character vector specifying columns to check for LLOQ for 2/3 data points.
#' Default is NULL, is used only for aggregationFlag "Custom".
#'
#' @return A `data.table` containing aggregated observed data.
#' @export
aggregateObservedDataGroups <- function(dataObserved,
                                        groups = NULL,
                                        aggregationFlag = c(
                                          "GeometricStdDev",
                                          "ArithmeticStdDev",
                                          "Percentiles",
                                          "Custom"
                                        ),
                                        percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1, 3, 5)],
                                        groupSuffix = "aggregated",
                                        customFunction = NULL,
                                        lloqCheckColumns2of3 = NULL,
                                        lloqCheckColumns1of2 = NULL) {
  dataToAggregate <- prepareDataForAggregation(
    dataObserved = dataObserved,
    groups = groups,
    groupSuffix = groupSuffix
  )
  if (is.null(dataToAggregate)) {
    return(NULL)
  }

  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction)

  aggregatedData <- performAggregation(
    dataToAggregate = dataToAggregate,
    aggregationFun = aggregationFun,
    aggrCriteria = c("group", "outputPathId", "xValues")
  )

  aggregatedData <- checkLLOQ(
    aggregatedData = aggregatedData,
    lloqCheckColumns2of3 = lloqCheckColumns2of3,
    lloqCheckColumns1of2 = lloqCheckColumns1of2,
    aggregationFlag = aggregationFlag
  )

  aggregatedData <- addUniqueColumns(dataToAggregate, aggregatedData)
  aggregatedData$dataClass <- DATACLASS$tpAggregated

  aggregatedData <- setDataTypeAttributes(aggregatedData)

  return(aggregatedData)
}

#' Prepares data for aggregation
#'
#' @inheritParams aggregateObservedDataGroups
#'
#' @return Prepared data for aggregation as a `data.table`.
#' @keywords internal
prepareDataForAggregation <- function(dataObserved, groups, groupSuffix) {
  # avoid warning for global variable
  group <- NULL

  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  checkmate::assertDataTable(dataObserved, min.rows = 1)
  checkmate::assertNames(names(dataObserved),
    must.include = c(
      "xValues", "yValues", "lloq",
      "individualId", "outputPathId", "group",
      "dataType", "dataClass"
    )
  )
  checkmate::assertChoice(unique(dataObserved$dataClass), choices = DATACLASS$tpIndividual)

  groups <- getIndividualDataGroups(dataObserved, groups)

  if (length(groups) == 0) {
    warning("No groups available for aggregation")
    return(NULL)
  }

  dataToAggregate <- dataObserved[group %in% groups]
  dataToAggregate[, group := paste(group, groupSuffix, sep = "_")]

  checkmate::assertNames(unique(dataToAggregate$group),
    disjunct.from = unique(dataObserved$group), # nolint indentation_linter
    .var.name = "new group names"
  )

  return(dataToAggregate)
}

#' Sets values which do not match the LLOQ criteria to NA
#'
#' @param aggregatedData A `data.table` with aggregated data.
#' @inheritParams aggregateObservedDataGroups
#'
#' @return Updated aggregatedData `data.table`.
#' @keywords internal
checkLLOQ <- function(aggregatedData, lloqCheckColumns2of3, lloqCheckColumns1of2, aggregationFlag) {
  # initialize data.table variables
  nBelowLLOQ <- numberOfPatients <- NULL

  if (aggregationFlag != "Custom" &
    (!is.null(lloqCheckColumns2of3) | !is.null(lloqCheckColumns1of2))) {
    warning(paste("input variable lloqCheckColumns2of3 and lloqCheckColumns1of2 are not used for aggregationFlag", aggregationFlag))
    lloqCheckColumns2of3 <- NULL
    lloqCheckColumns1of2 <- NULL
  }

  if (aggregationFlag %in% ospsuite::DataErrorType) {
    lloqCheckColumns2of3 <- c("yValues", "yErrorValues")
  } else if (aggregationFlag %in% "Percentiles") {
    lloqCheckColumns1of2 <- c("yValues", "yMin", "yMax")
  } else {
    if (is.null(lloqCheckColumns2of3) & is.null(lloqCheckColumns1of2)) {
      stop("For custom aggregation please provide lloqCheckColumns2of3 or lloqCheckColumns1of2")
    }
  }

  if (length(lloqCheckColumns2of3) > 0) {
    aggregatedData[, (lloqCheckColumns2of3) := lapply(.SD, function(x) {
      ifelse((nBelowLLOQ / numberOfIndividuals) > (2 / 3), NA, x)
    }),
    .SDcols = lloqCheckColumns2of3, by = .I
    ]
  }

  if (length(lloqCheckColumns1of2) > 0) {
    aggregatedData[, (lloqCheckColumns1of2) := lapply(.SD, function(x) {
      ifelse((nBelowLLOQ / numberOfIndividuals) > (1 / 2), NA, x)
    }),
    .SDcols = lloqCheckColumns1of2, by = .I
    ]
  }

  return(aggregatedData)
}

#' Add all columns of `dataObserved` which are unique for a group
#'
#' @param dataObserved The observed data.
#' @param aggregatedData The aggregated data.
#'
#' @return The updated aggregated data as a `data.table`.
#' @keywords internal
addUniqueColumns <- function(dataObserved, aggregatedData) {
  identifier <- c("group", "outputPathId")

  colsToCheck <- setdiff(names(dataObserved), identifier)
  columnISUnique <- dataObserved[, lapply(.SD, function(x) length(unique(x))),
    by = identifier, # nolint indentation_linter
    .SDcols = colsToCheck
  ] %>%
    .[, lapply(.SD, function(x) all(x == 1)), .SDcols = colsToCheck] %>%
    unlist()

  tmp <- dataObserved %>%
    dplyr::select(dplyr::all_of(c(identifier, colsToCheck[columnISUnique]))) %>%
    unique()

  aggregatedData <- merge(aggregatedData,
    tmp, # nolint indentation_linter
    by = identifier,
    all.x = TRUE
  )

  return(aggregatedData)
}

# Auxiliaries ---------

#' Sets columnType attribute according to dictionary
#'
#' @param dataDT A `data.table` with observed data.
#' @param dict Named list with columnNames and columnTypes; if NULL,
#'  list is produced by template saved in package installation.
#'
#' @return A `data.table` with attributes.
#' @keywords internal
setDataTypeAttributes <- function(dataDT, dict = NULL) {
  if (is.null(dict)) {
    tmpdict <-
      xlsxReadData(
        wb = system.file(
          "templates",
          "DataImportConfiguration.xlsx",
          package = "ospsuite.reportingframework",
          mustWork = TRUE
        ),
        "tpDictionary",
        skipDescriptionRow = TRUE
      ) %>%
      dplyr::select(c("targetColumn", "type")) %>%
      unique()
    dict <-
      as.list(tmpdict$type) %>%
      stats::setNames(tmpdict$targetColumn)
  }

  # Add dictionary as attributes
  for (dc in c("dataType", "dataClass")) {
    dict[[dc]] <- "identifier"
  }

  # Add columns used in class DataCombined
  for (dc in c("xUnit", "xDimension", "yDimension", "molWeight")) {
    dict[[dc]] <- "timeprofile"
  }

  # All unknown to covariates
  covariates <- setdiff(names(dataDT), names(dict))
  for (dc in covariates) {
    dict[[dc]] <- "covariates"
  }

  lapply(names(dataDT), function(dc) {
    data.table::setattr(dataDT[[dc]], "columnType", dict[[dc]])
  })

  return(dataDT)
}

#' Select all columns where the attribute `columnType` matches the requirement
#'
#' @param dt A `data.table` with attributes (e.g., imported by `readObservedDataByDictionary`).
#' @param columnTypes A vector with required types.
#'
#' @return A vector with column names.
#' @export
getColumnsForColumnType <- function(dt, columnTypes) {
  columnsWithAttributes <- unlist(lapply(dt, attr, "columnType"))

  columnNames <-
    names(columnsWithAttributes[columnsWithAttributes %in% columnTypes])
  return(columnNames)
}

#' Update identifier columns in a data.table
#'
#' This function updates the specified identifier columns in a `data.table` by replacing commas with underscores.
#' It also checks for the presence of commas in the columns and generates a warning message if found.
#'
#' @param dt The input `data.table`.
#' @param identifierCols A character vector specifying the columns to be updated.
#' @return The updated `data.table`.
#' @keywords internal
convertIdentifierColumns <- function(dt, identifierCols) {
  for (col in identifierCols) {
    dt[[col]] <- as.character(dt[[col]])
    if (any(grepl(",", dt[[col]]))) {
      warning(paste("Warning: Column", col, "commas were replaced by _"))
    }
    dt[[col]] <- gsub(",", "_", dt[[col]])
  }
  return(dt)
}


#' Filters observed data for individual groups which are suited for
#' aggregation or "Virtual Twin population" creation
#'
#' @inheritParams aggregateObservedDataGroups
#' @param minN The minimal number needed for a group.
#'
#' @return A vector with suitable group Ids.
#' @keywords internal
getIndividualDataGroups <- function(dataObserved, groups, minN = 2) {
  # avoid warnings for global variables
  individualId <- dataClass <- dataType <- N <- NULL # nolint object_name_linter

  tmp <-
    dataObserved[, .(N = dplyr::n_distinct(individualId)),
      by = c("group", "dataClass", "dataType")
    ] # nolint indentation_linter
  groupsAvailable <- unique(tmp[dataClass == DATACLASS$tpIndividual &
    dataType == "observed" &
    N >= minN]$group)


  if (is.null(groups)) {
    groups <- groupsAvailable
  } else {
    unsuitableGroups <- setdiff(groups, groupsAvailable)
    if (length(unsuitableGroups) > 0) {
      warning(paste(
        "Groups", paste(unsuitableGroups, collapse = ", "), "are not suited for grouping.",
        "Check if they are available in data, have more then", minN, "Individuals or
                    if they are have data class", DATACLASS$tpIndividual
      ))
    }
    groups <- intersect(groups, groupsAvailable)
  }

  return(groups)
}
