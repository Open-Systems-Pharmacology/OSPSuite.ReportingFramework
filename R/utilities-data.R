#' Read data by dictionary
#'
#' This function reads and processes data based on the provided project configuration.
#'
#' @param projectConfiguration The project configuration
#' @param dataType type of data, available `r paste0('"',paste(unlist(DATATYPE),collapse = '", "'),'"')`
#'
#' @return Processed data based on the dictionary
#' @export
readObservedDataByDictionary <- function(projectConfiguration,
                                         dataType = unlist(DATATYPE)) { # nolint
  checkmate::assertFileExists(projectConfiguration$dataImporterConfigurationFile)
  dataType <- match.arg(dataType)

  logCatch({
    dataList <- xlsxReadData(wb = projectConfiguration$dataImporterConfigurationFile, sheetName =  "DataFiles")
    # delete description line and empty lines
    dataList <- dataList[-1, ]
    dataList <- dataList[rowSums(is.na(dataList)) < ncol(dataList), ]

    # filter for DataType
    dataList <- dataList[dataList$DataType == dataType]

    # loop on selected datafiles
    data <- data.table()
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

      data <- rbind(data,
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
    dict[["timeUnit"]] <- "timeprofile"
    for (dc in names(dict)) {
      data.table::setattr(data[[dc]], "columnType", dict[[dc]])
    }

    validateObservedData(data, stopIfValidationFails = FALSE)

    # update sheet DataGroupId
    updateDataGroupId(projectConfiguration, data)
  })

  return(data)
}


#' Validate observed data
#'
#' This function checks the validity of the observed dataset.
#'
#' @param data The observed dataset
#' @param stopIfValidationFails Flag to indicate whether to stop if validation fails
validateObservedData <- function(data, stopIfValidationFails = TRUE) {
  # initialize variables used fo data.tables
  dvUnit <- NULL

  .returnMessage <- function(msg, stopIfValidationFails) {
    if (stopIfValidationFails) stop(msg)
    warning(msg)
  }

  # check data validity
  colIdentifier <-
    c("individualId", "groupId", "outputPathId", "time")
  if (any(duplicated(data %>%
                     dplyr::select(dplyr::all_of(colIdentifier))))) {
    .returnMessage(
      paste(
        "data is not unique in columns",
        paste(colIdentifier, collapse = ", ")
      ),
      stopIfValidationFails
    )
  }
  for (col in setdiff(names(data), c("lloq", "dvUnit"))) {
    if (any(is.na(data[[col]]) | data[[col]] == "")) {
      .returnMessage(
        paste("data contains NAs or empty values in column", col),
        stopIfValidationFails
      )
      print(paste("empty entries in", col))
      print(data[is.na(get(col)) | get(col) == ""])
    }
  }
  colIdentifier <- c("groupId", "outputPathId")
  if (any(data[, .(N = dplyr::n_distinct(dvUnit)), by = "outputPathId"]$N > 1)) { # nolint
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

  dict <- xlsxReadData(wb = dictionaryFile, sheetName = sheet) %>%
    data.table::setDT()
  dict <- dict[-1, ]
  dict <- dict[rowSums(is.na(dict)) < ncol(dict), ]
  dict[,targetColumn := trimws(targetColumn)]
  dict[,sourceColumn := trimws(sourceColumn)]
  dict[,sourceColumn := ifelse(sourceColumn == '',NA,sourceColumn)]

  # filter for Type
  if (dataType == DATATYPE$individual) {
    dict <- dict[!(targetColumn %in% c("yErrorValues", "yErrorType", "n_belowLLOQ")), ]
  }

  checkmate::assertNames(
    dict$targetColumn,
    must.include = c(
      "individualId",
      "groupId",
      "outputPathId",
      "time",
      "dv",
      "dvUnit"
    ), .var.name = paste("Check for missing targetColumns in  dictionary", sheet)
  )

  tmp <- dict[is.na(sourceColumn) & is.na(filter), ]
  if (nrow(tmp) > 0) {
    stop(paste("Either sourceColumn or Filter on sourceColumn has to be filled in dictionary", sheet))
  }

  checkmate::assertNames(
    x = dict[!is.na(sourceColumn)]$sourceColumn,
    subset.of = names(data)
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
  targetColumn <- sourceColumn <- timeUnit <- NULL

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
  data[, timeUnit := dict[targetColumn == "time"]$sourceUnit]


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

#' Add group ids of observed data to configration sheet
#'
#' @param projectConfiguration The project configuration
#' @param data `data.table`with observed Data
updateDataGroupId <- function(projectConfiguration, data) {
  # initialize variables used fo data.tables
  studyId <- groupId <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)

  dtDataGroupIds <- xlsxReadData(wb = wb, sheetName = "DataGroupID")

  dtDataGroupIdsNew <- data[, c("studyId", "groupId")] %>%
    unique() %>%
    dplyr::mutate(studyId = as.character(studyId)) %>%
    dplyr::mutate(groupId = as.character(groupId))

  dtDataGroupIds <- rbind(dtDataGroupIds,
    dtDataGroupIdsNew,
    fill = TRUE
  )

  dtDataGroupIds <- dtDataGroupIds[!duplicated(dtDataGroupIds[, c("studyId", "groupId")])]

  xlsxWriteData(wb = wb, sheetName = "DataGroupID", dt = dtDataGroupIds)
  openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)

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
      groups = as.character(groupData$groupId[1])
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
  .groupBy <- NULL

  # group data by identifier
  groupBy <- getColumnsForColumnType(dt = dataDT, columnTypes = "identifier")

  # use a copy, to keep the data.table outside the function unchanged
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
    xValues = groupData$time,
    yValues = groupData$dv
  )

  if (dplyr::n_distinct(groupData$dvUnit) > 1) {
    stop(paste(
      "DataDT to combinedData: dv Unit for dataset",
      groupName, "is not unique"
    ))
  }
  dataSet$yUnit <- groupData$dvUnit[1]

  if (dplyr::n_distinct(groupData$timeUnit) > 1) {
    stop(paste(
      "DataDT to combinedData: time Unit for dataset",
      groupName, "is not unique"
    ))
  }
  dataSet$xUnit <- groupData$timeUnit[1]

  if (all(is.na(groupData$lloq))) {
    lLOQ <- NA
  } else {
    lLOQ <- groupData[!is.na(lloq)]$lloq
    if (dplyr::n_distinct(lLOQ) > 1) {
      warning(paste(
        "DataDT to combinedData: More then one LLOQ for dataset",
        groupName,
        "is set to minimal"
      ))
    }
    lLOQ <- min(lLOQ)
  }
  dataSet$LLOQ <- lLOQ


  return(dataSet)
}

#' add meta data to a data set
#'
#' @param dataSet `DataSet` with observed data
#' @param groupData corresponding datatable with metadat
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



#' add Biometric information to config
#'
#' @inheritParams source readObservedDataByDictionary
#' @param observedData `data.table` with oberved data
#'#' @export
addBiometricsToConfig <- function(projectConfiguration, observedData, overwrite = FALSE) {
  checkmate::assertFileExists(projectConfiguration$individualsFile)

  # initialize variables used fo data.tables
  gender <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

  dtIndividualBiometrics <- xlsxReadData(wb = wb,sheetName = "IndividualBiometrics")

  biometrics <-
    observedData %>%
    dplyr::select(
      c(
        "individualId",
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


