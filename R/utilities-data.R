#' Read data by dictionary
#'
#' This function reads and processes data based on the provided project configuration.
#'
#' @param projectConfiguration The project configuration
#' @param dataType type of data, available `r paste0('"',paste(unlist(DATATYPE),collapse = '", "'),'"')`
#' @param addBiometricsToConfigFlag if TRUE inividual biometrics are added  to Individualxlsx,
#    needed if you want to create individuals bases on this data
#'
#' @return Processed data based on the dictionary
#' @export
readObservedDataByDictionary <- function(projectConfiguration,
                                         dataType = unlist(DATATYPE),
                                         addBiometricsToConfigFlag = TRUE) { # nolint
  checkmate::assertFileExists(projectConfiguration$dataImporterConfigurationFile)
  dataType <- match.arg(dataType)

  dataList <- xlsxReadData(wb = projectConfiguration$dataImporterConfigurationFile,
                           sheetName =  "DataFiles",
                           skipDescriptionRow = TRUE)

  # filter for DataType
  dataList <- dataList[dataList$DataType == dataType]

  # loop on selected datafiles
  dataDT <- data.table()
  dict <- list()
  for (d in split(dataList, seq_len(nrow(dataList)))) {
    tmpData <- data.table::fread(fs::path_abs(
      start = projectConfiguration$projectConfigurationDirPath,
      path = d$DataFile
    ))

    tmpdict <- readDataDictionary(
      dictionaryFile = projectConfiguration$dataImporterConfigurationFile,
      sheet = d$Dictionary,
      data = tmpData,
      dataType = dataType
    )

    dataDT <- rbind(dataDT,
                  convertDataByDictionary(
                    data = tmpData,
                    dataFilter = d$DataFilter,
                    dict = tmpdict,
                    dictionaryName = d$Dictionary
                  ),
                  fill = TRUE
    )

    # get unique dictionary for columnType
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

  # add dictionary as attributes
  dict[["xUnit"]] <- "timeprofile"
  lapply(names(dict), function(dc) {
    data.table::setattr(dataDT[[dc]], "columnType", dict[[dc]])
  })

  validateObservedData(dataDT = dataDT, stopIfValidationFails = FALSE)

  # spread data to other tables
  updateDataGroupId(projectConfiguration = projectConfiguration,
                    dataDT = dataDT)

  updateOutputPathId(projectConfiguration = projectConfiguration,
                     dataDT = dataDT)

  if (addBiometricsToConfigFlag)
    addBiometricsToConfig(observedData = dataDT,
                          projectConfiguration = projectConfiguration)

  return(dataDT)
}


#' Validate observed data
#'
#' This function checks the validity of the observed dataset.
#'
#' @param data The observed dataset
#' @param stopIfValidationFails Flag to indicate whether to stop if validation fails
validateObservedData <- function(dataDT, stopIfValidationFails = TRUE) {
  # initialize variables used fo data.tables
  yUnit <- NULL

  .returnMessage <- function(msg, stopIfValidationFails) {
    if (stopIfValidationFails) stop(msg)
    warning(msg)
  }

  # check colIdentifier
  columnsWithAttributes <- lapply(dataDT, attr, "columnType")
  columnsWithAttributes <-
    names(columnsWithAttributes)[unlist(lapply(columnsWithAttributes, function(col) {
      !is.null(col)
    }))]
  if (!all(names(dataDT) %in% columnsWithAttributes))
    .returnMessage(
      paste0(
        'Some data columns have no attribute: "',
        paste(setdiff(names(dataDT),columnsWithAttributes), collapse = '", "'),
        '"'
      ),
      stopIfValidationFails
    )
  # check data validity
  colIdentifier <-
    c("IndividualId", "group", "OutputPathId", "xValues")
  if (any(duplicated(dataDT , by = colIdentifier))) {
    .returnMessage(
      paste(
        "data is not unique in columns",
        paste(colIdentifier, collapse = ", ")
      ),
      stopIfValidationFails
    )
  }
  for (col in setdiff(names(dataDT), c("lloq", "yUnit"))) {
    if (any(is.na(dataDT[[col]]) | dataDT[[col]] == "")) {
      .returnMessage(
        paste("data contains NAs or empty values in column", col),
        stopIfValidationFails
      )
      print(paste("empty entries in", col))
      print(dataDT[is.na(get(col)) | get(col) == ""])
    }
  }
  colIdentifier <- c("group", "OutputPathId")
  if (any(dataDT[, .(N = dplyr::n_distinct(yUnit)), by = "OutputPathId"]$N > 1)) { # nolint
    .returnMessage(
      paste("data is not unique in columns", paste(colIdentifier, collapse = ", ")),
      stopIfValidationFails
    )
  }
}

#' Read data dictionary
#'
#' This function reads the data dictionary based on the provided file and sheet.
#'
#' @param dictionaryFile The file containing the data dictionary
#' @param sheet The sheet within the data dictionary file
#' @param dataType type of data
#' @param data The data to be used with the dictionary
#'
#' @return The data dictionary
readDataDictionary <- function(dictionaryFile, sheet, data, dataType) {
  # initialize variables used fo data.tables
  sourceColumn <- filter <- targetColumn <- NULL

  dict <- xlsxReadData(wb = dictionaryFile, sheetName = sheet,skipDescriptionRow = TRUE)

  # filter for Type
  if (dataType == DATATYPE$individual) {
    dict <- dict[!(targetColumn %in% c("yErrorValues", "yErrorType", "nBelowLLOQ")), ]
  }

  checkmate::assertNames(
    dict$targetColumn,
    must.include = c(
      "IndividualId",
      "group",
      "OutputPathId",
      "xValues",
      "yValues",
      "yUnit"
    ), .var.name = paste("Check for missing targetColumns in  dictionary", sheet)
  )

  tmp <- dict[is.na(sourceColumn) & is.na(filter), ]
  if (nrow(tmp) > 0) {
    stop(paste("Either sourceColumn or Filter on sourceColumn has to be filled in dictionary", sheet))
  }

  checkmate::assertNames(
    x = dict[!is.na(sourceColumn)]$sourceColumn,
    subset.of = names(data),
    .var.name = paste('Source column of',sheet)
  )

  return(dict)
}


#' Convert data by dictionary
#'
#' This function converts the data based on the provided dictionary and filters.
#'
#' @param data The data to be converted
#' @param dataFilter The filter to be applied to the data
#' @param dict The dictionary to be used for conversion
#' @param dictionaryName The name of the dictionary
#' @return The converted data
convertDataByDictionary <- function(data,
                                    dataFilter,
                                    dict,
                                    dictionaryName) {
  # initialize variables used fo data.tables
  targetColumn <- sourceColumn <- xUnit <- NULL

  # execute data Filter
  if (!is.na(dataFilter) & dataFilter != "") data <- data[eval(parse(text = dataFilter))]

  # execute all filters
  if (any(!is.na(dict$filter))) {
    dictFilters <- dict[!is.na(filter)]

    for (myFilter in split(dictFilters, seq_len(nrow(dictFilters)))) {
      data[
        eval(parse(text = myFilter$filter)),
        (myFilter$targetColumn) := eval(parse(text = myFilter$filterValue))
      ]
    }
  }

  # Rename columns to target columns
  dictColumns <- dict[!is.na(sourceColumn) & sourceColumn != targetColumn]

  checkmate::assertCharacter(
    dictColumns$targetColumn,
    unique = TRUE,
    .var.name = paste("target columns with source columnsof", dictionaryName)
  )

  # Create new columns for duplicated old names, do not use setnames as source columns may not be unique
  for (iRow in seq_len(nrow(dictColumns))) {
    data[, (dictColumns$targetColumn[iRow]) := data[[dictColumns$sourceColumn[iRow]]]]
  }

  # reduce to defined columns
  data <- data %>%
    dplyr::select(unique(dict$targetColumn))

  # add time unit
  data[, xUnit := dict[targetColumn == "xValues"]$sourceUnit]

  # transfer identifier to character
  identifierCols <- dict[type == 'identifier']$targetColumn
  data[, (identifierCols) := lapply(.SD, as.character), .SDcols = identifierCols]

  data <- convertBiometrics(data,dict)

  return(data)
}


#' converts biomertic columns to default unit
#'
#' @inheritParams convertDataByDictionary
#'
#' @return `data.table` with converted columns
convertBiometrics <- function(data, dict,dictionaryName) {

  # initialize variables used fo data.tables
  targetColumn <- NULL

  biometricUnits <- list(
    age = "year(s)",
    weight = "kg",
    height = "cm"
  )

  for (col in intersect(names(biometricUnits), dict$targetColumn)) {
    unitFactor <- ospsuite::toUnit(
      quantityOrDimension = ospsuite::getDimensionForUnit(biometricUnits[[col]]),
      values = 1,
      targetUnit = biometricUnits[[col]],
      sourceUnit = dict[targetColumn == col]$sourceUnit[1]
    )

    data[, (col) := get(col) * unitFactor]
  }

  if ('gender' %in% dict$targetColumn){

    data[,gender := ifelse(gender == 1,'MALE',gender)]
    data[,gender := ifelse(gender == 2,'FEMALE',gender)]
    data[,gender := toupper(gender)]

    data[,gender := ifelse(gender != 'MALE' & gender != 'FEMALE','UNKNOWN',gender)]

    if (any(data$gender == 'UNKNOWN'))
      warning(paste('Unknown gender in data set'))

  }

  return(data)
}

#' Add group ids of observed data to configuration sheet
#'
#' @param projectConfiguration The project configuration
#' @param data `data.table`with observed Data
updateDataGroupId <- function(projectConfiguration, dataDT) {
  # initialize variables used fo data.tables
  StudyId <- group <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)

  dtDataGroupIds <- xlsxReadData(wb = wb, sheetName = "DataGroupID")

  dtDataGroupIdsNew <- dataDT[, c("StudyId", "group")] %>%
    unique() %>%
    dplyr::mutate(StudyId = as.character(StudyId)) %>%
    dplyr::mutate(group = as.character(group))

  dtDataGroupIds <- rbind(dtDataGroupIds,
                          dtDataGroupIdsNew,
                          fill = TRUE
  )

  dtDataGroupIds <- dtDataGroupIds[!duplicated(dtDataGroupIds[, c("StudyId", "group")])]

  xlsxWriteData(wb = wb, sheetName = "DataGroupID", dt = dtDataGroupIds)
  openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)

  return(invisible())
}


#' Add outputpath ids of observed data to configuration sheet
#'
#' @param projectConfiguration The project configuration
#' @param data `data.table`with observed Data
updateOutputPathId <- function(projectConfiguration, dataDT) {
  # initialize variables used fo data.tables
  OutputPathId <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenarioDefinitionFile)

  dtOutputPaths <- xlsxReadData(wb = wb, sheetName = "OutputPaths")

  dtOutputPathsNew <- dataDT[, c("OutputPathId")] %>%
    unique() %>%
    dplyr::mutate(OutputPathId = as.character(OutputPathId))  %>%
    data.table::setnames('OutputPathId','OutputPathId')

  dtOutputPaths <- rbind(dtOutputPaths,
                          dtOutputPathsNew,
                          fill = TRUE
  )

  dtOutputPaths <- dtOutputPaths[!duplicated(dtOutputPaths,by = 'OutputPathId')]

  xlsxWriteData(wb = wb, sheetName = "OutputPaths", dt = dtOutputPaths)
  openxlsx::saveWorkbook(wb, projectConfiguration$scenarioDefinitionFile, overwrite = TRUE)

  return(invisible())
}


#' coverts data.table with observed to `DataCombined` object
#'
#' data.table must be formatted like a table produced by `readObservedDataByDictionary`
#'
#' @param dataDT `data.table`to convert
#'
#' @return object of class `DataCombined`
#' @export
convertDataTableToDataCombined <- function(dataDT) {

  if (any(is.null(unlist(lapply(dataDT, attr, "columnType"))))) {
    stop(paste(
      "There are some data columns without attribute columnType.",
      "Please use a table generated by readObservedDataByDictionary"
    ))
  }

  groupedData <- groupDataByIdentifier(dataDT)

  dataCombined <- ospsuite::DataCombined$new()

  for (groupData in groupedData) {
    dataSet <- createDataSets(groupData)
    dataSet <- addMetaDataToDataSet(dataSet, groupData)

    dataCombined$addDataSets(
      dataSets = dataSet,
      groups = as.character(groupData$displayName[1])
    )
  }

  return(dataCombined)
}


#' groups the data by identifier
#'
#' @param dataDT `data.table` to be grouped
#'
#' @return list with grouped data
groupDataByIdentifier <- function(dataDT) {
  checkmate::assert_disjunct(names(dataDT), ".groupBy")
  checkmate::assert_disjunct(names(dataDT), "displayName")
  .groupBy <- displayName <- NULL


  dtDataGroupIds <- xlsxReadData(wb = projectConfiguration$dataImporterConfigurationFile,
                                 sheetName = 'DataGroupID')

  checkmate::assertCharacter(dtDataGroupIds[!is.na(dtDataGroupIds$displayName)]$displayName,
                             unique = TRUE,.var.name = 'display Name of data group')

  dataDT <- data.table::copy(dataDT) %>%
    merge(dtDataGroupIds,
          by = c('StudyId','group'),
          all.x = TRUE)
  lapply(c('StudyId','group'), function(col) {
    data.table::setattr(dataDT[[col]], "columnType", 'identifier')
  })


  # group data by identifier
  groupBy <- getColumnsForColumnType(dt = dataDT, columnTypes = "identifier")

  # use a copy, to keep the data.table outside the function unchanged
  dataDT <- data.table::copy(dataDT) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::all_of(groupBy))) %>%
    dplyr::mutate(.groupBy = paste(!!!dplyr::syms(groupBy), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(displayName := ifelse(is.na(displayName),.groupBy,displayName))

  groupedData <- dataDT %>%
    dplyr::group_split(.groupBy)

  # Create a named list with.groupBy as names
  names(groupedData) <- unlist(lapply(groupedData, function(x) {
    unique(x[[".groupBy"]])
  }))

  return(groupedData)
}


#' select all columns where the attribute `columnType` matches the requirement
#'
#' @param dt `data.table` with attributes (e.g. imported by `readObservedDataByDictionary`)
#' @param columnTypes vector with required types
#'
#' @return vector with column names
#' @export
getColumnsForColumnType <- function(dt, columnTypes) {
  columnsWithAttributes <- unlist(lapply(dt, attr, "columnType"))

  columnNames <-
    names(columnsWithAttributes[columnsWithAttributes %in% columnTypes])
  return(columnNames)
}


#' Function to create data sets from grouped data
#'
#' @param groupData data.table unique for identifier
#'
#' @return object of class 'DataSet'
createDataSets <- function(groupData) {
  # initialize variables used fo data.tables
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
      "DataDT to combinedData: dv Unit for dataset",
      groupName, "is not unique"
    ))
  }
  dataSet$yDimension <- ospsuite::getDimensionForUnit(groupData$yUnit[1])
  dataSet$yUnit <- groupData$yUnit[1]

  if (dplyr::n_distinct(groupData$xUnit) > 1) {
    stop(paste(
      "DataDT to combinedData: time Unit for dataset",
      groupName, "is not unique"
    ))
  }
  dataSet$xUnit <- groupData$xUnit[1]

  if (any(!is.na(groupData$lloq))) {
    lLOQ <- groupData[!is.na(lloq)]$lloq
    if (dplyr::n_distinct(lLOQ) > 1) {
      warning(paste(
        "DataDT to combinedData: More then one LLOQ for dataset",
        groupName,
        "is set to minimal"
      ))
    }
    lLOQ <- min(lLOQ)
    dataSet$LLOQ <- lLOQ
  }


  return(dataSet)
}

#' add meta data to a data set
#'
#' @param dataSet `DataSet` object with observed data
#' @param groupData corresponding `data.table` with meta data
#'
#' @return `DataSet` with observed data with added metadata
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



#' add bio-metrics   information to config
#'
#' @param projectConfiguration The project configuration
#' @param observedData `data.table` with observed data
#' @param overwrite if TRUE existing rows will be overwritten
#'
#' @export
addBiometricsToConfig <- function(projectConfiguration, observedData, overwrite = FALSE) {
  checkmate::assertFileExists(projectConfiguration$individualsFile)

  # initialize variables used fo data.tables
  gender <- NULL

  validateObservedData(dataDT = dataDT, stopIfValidationFails = FALSE)

  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

  dtIndividualBiometrics <- xlsxReadData(wb = wb,sheetName = "IndividualBiometrics")

  biometrics <-
    observedData %>%
    dplyr::select(
      c(
        "IndividualId",
        names(observedData)[unlist(lapply(observedData, attr, "columnType")) == "biometrics"]
      )
    ) %>%
    unique()

  for (col in names(biometrics)) {
    newName <- grep(col, names(dtIndividualBiometrics), ignore.case = TRUE, value = TRUE)
    if (newName != "") {
      data.table::setnames(biometrics, old = col, new = newName)
    }
  }

  if (!("Species" %in% names(biometrics))) biometrics[["Species"]] <- ospsuite::Species$Human

  # merge old an new tables
  dtIndividualBiometrics <-
    rbind(dtIndividualBiometrics,
          biometrics,
          fill = TRUE
    )

  # if overwrite FALSE take original located at the top, otherwise take new rows located at the bottom
  dtIndividualBiometrics <-
    dtIndividualBiometrics[!duplicated(dtIndividualBiometrics,
                                       by = 'IndividualId',
                                       fromLast = overwrite)]

  xlsxWriteData(wb = wb, sheetName = "IndividualBiometrics", dt = dtIndividualBiometrics)

  openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

  return(invisible())
}


