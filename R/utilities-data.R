# Read data by dictionary ----------

#' Read data by dictionary
#'
#' This function reads and processes data based on the provided project configuration.
#'
#' @template projectConfig
#' @param addBiometricsToConfigFlag if TRUE inividual biometrics are added  to Individualxlsx,
#    needed if you want to create individuals bases on this data
#'
#' @return Processed data based on the dictionary
#' @export
readObservedDataByDictionary <- function(projectConfiguration,
                                         addBiometricsToConfigFlag = TRUE) { # nolint
  checkmate::assertFileExists(projectConfiguration$dataImporterConfigurationFile)

  dataList <- xlsxReadData(
    wb = projectConfiguration$dataImporterConfigurationFile,
    sheetName = "DataFiles",
    skipDescriptionRow = TRUE
  )

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
      dataClass = d$DataClass
    )

    dataDT <- rbind(dataDT,
      convertDataByDictionary(
        data = tmpData,
        dataFilter = d$DataFilter,
        dict = tmpdict,
        dictionaryName = d$Dictionary
      ) %>%
        dplyr::mutate(DataClass = d$DataClass),
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

  dataDT[, dataType := "observed"]

  dataDT <- setDataTypeAttributes(dataDT, dict)

  validateObservedData(dataDT = dataDT, stopIfValidationFails = FALSE)

  # spread data to other tables
  updateDataGroupId(
    projectConfiguration = projectConfiguration,
    dataDT = dataDT
  )

  updateOutputPathId(
    projectConfiguration = projectConfiguration,
    dataDT = dataDT
  )

  if (addBiometricsToConfigFlag) {
    addBiometricsToConfig(
      dataDT = dataDT,
      projectConfiguration = projectConfiguration
    )
  }

  # logging
  message('Observed Data:')
  writeTableToLog(dataDT[,.('No of data points' = .N,
                            'No of individuals' = dplyr::n_distinct(IndividualId),
                            'No of outputs' = dplyr::n_distinct(OutputPathId)),
                         by = c('group') ])

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
  if (!all(names(dataDT) %in% columnsWithAttributes)) {
    .returnMessage(
      paste0(
        'Some data columns have no attribute: "',
        paste(setdiff(names(dataDT), columnsWithAttributes), collapse = '", "'),
        '"'
      ),
      stopIfValidationFails
    )
  }
  # check data validity
  colIdentifier <-
    intersect(c("IndividualId", "group", "OutputPathId", "xValues"),
              names(dataDT))
  if (any(duplicated(dataDT, by = colIdentifier))) {
    .returnMessage(
      paste(
        "data must be unique in columns",
        paste(colIdentifier, collapse = ", ")
      ),
      stopIfValidationFails
    )
  }
  for (col in setdiff(names(dataDT), c(
    "lloq", "yUnit",
    "yErrorValues", "yErrorType", "nBelowLLOQ"
  ))) {
    if (any(is.na(dataDT[[col]]) | dataDT[[col]] == "")) {
      .returnMessage(
        paste("data contains NAs or empty values in column", col),
        FALSE
      )
      print(paste("empty entries in", col))
    }
  }
  colIdentifier <- c("group", "OutputPathId")
  if (any(dataDT[, .(N = dplyr::n_distinct(yUnit)), by = "OutputPathId"]$N > 1)) { # nolint
    .returnMessage(
      paste("dv unit is ambiguous in columns", paste(colIdentifier, collapse = ", ")),
      stopIfValidationFails
    )
  }

  if ("yErrorType" %in% names(dataDT)){
    if (any(dataDT$yErrorType %in%  unlist(ospsuite::DataErrorType))){
      checkmate::assertNames(
        names(dataDT),
        must.include = "yErrorValues")
    } else {
      checkmate::assertNames(
        names(dataDT),
        must.include = c("yMin","yMax"))
    }

  }
}

#' Read data dictionary
#'
#' This function reads the data dictionary based on the provided file and sheet.
#'
#' @param dictionaryFile The file containing the data dictionary
#' @param sheet The sheet within the data dictionary file
#' @param data The data to be used with the dictionary
#' @param dataClass class of data either "tp Individual" or "tp Aggregated"
#'
#' @return The data dictionary
readDataDictionary <-
  function(dictionaryFile,
           sheet,
           data,
           dataClass = grep('^tp',unlist(DATACLASS),value = TRUE)) {
    # initialize variables used fo data.tables
    sourceColumn <- filter <- targetColumn <- NULL

    dict <- xlsxReadData(wb = dictionaryFile, sheetName = sheet, skipDescriptionRow = TRUE)

    dataClass <- match.arg(dataClass)

    if (dataClass == DATACLASS$tpIndividual){
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
    } else if (dataClass == DATACLASS$tpAggregated){
      checkmate::assertNames(
        dict$targetColumn,
        must.include = c(
          "group",
          "OutputPathId",
          "xValues",
          "yValues",
          "yUnit",
          "yErrorType",
          "nBelowLLOQ"
        ), .var.name = paste("Check for missing targetColumns in  dictionary", sheet)
      )
    }

  tmp <- dict[is.na(sourceColumn) & is.na(filter), ]
  if (nrow(tmp) > 0) {
    stop(paste0('Either sourceColumn or Filter on sourceColumn has to be filled in dictionary "', sheet,
               '" for targetColumn(s) "', paste(tmp$targetColumn,collapse = '", "'),'"'))
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

  # transfer identifier to character without commas
  data <- convertIdentifierColumns(dt = data,
                                   identifierCols = dict[type == "identifier"]$targetColumn)

  data <- convertBiometrics(data, dict)

  return(data)
}


#' converts biomertic columns to default unit
#'
#' @inheritParams convertDataByDictionary
#'
#' @return `data.table` with converted columns
convertBiometrics <- function(data, dict, dictionaryName) {
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

  if ("gender" %in% dict$targetColumn) {
    data[, gender := ifelse(gender == 1, "MALE", gender)]
    data[, gender := ifelse(gender == 2, "FEMALE", gender)]
    data[, gender := toupper(gender)]

    data[, gender := ifelse(gender != "MALE" & gender != "FEMALE", "UNKNOWN", gender)]

    if (any(data$gender == "UNKNOWN")) {
      warning(paste("Unknown gender in data set"))
    }
  }

  return(data)
}

#' Add group ids of observed data to configuration sheet
#'
#' @template projectConfig
#' @param data `data.table`with observed Data
updateDataGroupId <- function(projectConfiguration, dataDT) {
  # initialize variables used fo data.tables
  StudyId <- group <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dtDataGroupIds <- xlsxReadData(wb = wb, sheetName = "DataGroups")

  identifierCols <- intersect(names(dataDT),c("group","StudyId","StudyArm"))

  colsSelected <- unique(c(identifierCols,
    getColumnsForColumnType(dt = dataDT,columnTypes = 'metadata')))

  dtDataGroupIdsNew <- dataDT %>%
    dplyr::select(all_of(colsSelected)) %>%
    unique() %>%
    dplyr::mutate(StudyId = as.character(StudyId)) %>%
    dplyr::mutate(group = as.character(group))


  dtDataGroupIds <- rbind(dtDataGroupIds,
    dtDataGroupIdsNew,
    fill = TRUE
  )

  dtDataGroupIds <- dtDataGroupIds[!duplicated(dtDataGroupIds %>%
                                                 dplyr::select(all_of(identifierCols)))]


  xlsxWriteData(wb = wb, sheetName = "DataGroups", dt = dtDataGroupIds)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}


#' Add outputpath ids of observed data to configuration sheet
#'
#' @template projectConfig
#' @param data `data.table`with observed Data
updateOutputPathId <- function(projectConfiguration, dataDT) {
  # initialize variables used fo data.tables
  OutputPathId <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dtOutputPaths <- xlsxReadData(wb = wb, sheetName = "Outputs")

  dtOutputPathsNew <- dataDT[, c("OutputPathId")] %>%
    unique() %>%
    dplyr::mutate(OutputPathId = as.character(OutputPathId)) %>%
    data.table::setnames("OutputPathId", "OutputPathId")


  dtOutputPaths <- rbind(dtOutputPaths,
    dtOutputPathsNew,
    fill = TRUE
  )

  dtOutputPaths <- dtOutputPaths[!duplicated(dtOutputPaths, by = "OutputPathId")]


  xlsxWriteData(wb = wb, sheetName = "Outputs", dt = dtOutputPaths)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}

# converts data.table with observed data to `DataCombined` object ----------


#' converts data.table with observed data to `DataCombined` object
#'
#' data.table must be formatted like a table produced by `readObservedDataByDictionary`
#'
#' @param dataDT `data.table`to convert
#'
#' @return object of class `DataCombined`
#' @export
convertDataTableToDataCombined <- function(dataDT) {
  validateObservedData(dataDT = dataDT)

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
  # rename IndividualId to avoid conflict with simulated Results
  for (col in setdiff(metaColumns, "IndividualId")) {
    dataSet$addMetaData(name = col, value = as.character(metaData[[col]]))
  }
  dataSet$addMetaData(name = "IndividualIdObserved", value = "IndividualId")

  return(dataSet)
}



#' add bio-metrics   information to config
#'
#' @template projectConfig
#' @param dataDT `data.table` with observed data
#' @param overwrite if TRUE existing rows will be overwritten
#'
#' @export
addBiometricsToConfig <- function(projectConfiguration, dataDT, overwrite = FALSE) {
  checkmate::assertFileExists(projectConfiguration$individualsFile)

  # initialize variables used fo data.tables
  gender <- NULL

  validateObservedData(dataDT = dataDT, stopIfValidationFails = FALSE)

  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

  dtIndividualBiometrics <- xlsxReadData(wb = wb, sheetName = "IndividualBiometrics")

  biometrics <-
    dataDT %>%
    dplyr::select(
      c(
        "IndividualId",
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
      by = "IndividualId",
      fromLast = overwrite
    )]

  xlsxWriteData(wb = wb, sheetName = "IndividualBiometrics", dt = dtIndividualBiometrics)

  openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

  return(invisible())
}

# converts `DataCombined` object to data.table  ----------


#' converst object of class dataCombined to data.table with atttributes
#'
#' format correspondends to data.table produced by `readObservedDataByDictionary`
#' if the metaData does not contain `StudyId` StudyId 0 will be added.
#'
#' @param datacombined
#'
#' @return `data.table`
#' @export
convertDataCombinedToDataTable <- function(datacombined) {
  dataDT <- dataCombined$toDataFrame() %>%
    data.table::setDT()
  dataDT <- dataDT[dataType == "observed"]

  # delete columns not needed
  dataDT <- dataDT[, which(colSums(is.na(dataDT)) != nrow(dataDT)), with = FALSE]

  # set DataClass
  dataDT[,DataClass := ifelse(any(!is.na(yErrorValues)),DATACLASS$tpAggregated,DATACLASS$tpAggregated),
                              by:='group']

  # avoid conflict with population IndividualID
  data.table::setnames(dataDT, "IndividualIdObserved", "IndividualID", skip_absent = TRUE)
}

# auxiliaries ---------
#' sets comulnType attribute according to dictionary
#'
#' @param dataDT `data.table` with observed data
#' @param dict named list with columnNames and columntypes, if NULL,
#'  list is produced by template saved in package installation
#'
#' @return `data.table` with attributes
setDataTypeAttributes <- function(dataDT, dict = NULL) {
  if (is.null(dict)) {
    tmpdict <-
      xlsxReadData(
        wb = system.file(
          "templates",
          "templateProject",
          "Data",
          "dataImportConfiguration.xlsx",
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

  # add dictionary as attributes
  for (dc in c("dataType","DataClass")) {
    dict[[dc]] <- "identifier"
  }

  # add columns used in class DataCombined
  for (dc in c("xUnit", "xDimension", "yDimension", "molWeight")) {
    dict[[dc]] <- "timeprofile"
  }

  # all unknown to covariates
  covariates <- setdiff(names(dataDT), names(dict))
  for (dc in covariates) {
    dict[[dc]] <- "covariates"
  }

  lapply(names(dataDT), function(dc) {
    data.table::setattr(dataDT[[dc]], "columnType", dict[[dc]])
  })


  return(dataDT)
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


#' Update identifier columns in a data.table
#'
#' This function updates the specified identifier columns in a data.table by replacing commas with underscores.
#' It also checks for the presence of commas in the columns and generates a warning message if found.
#'
#' @param dt The input data.table
#' @param identifierCols A character vector specifying the columns to be updated
#' @return The updated data.table
#' @examples
#' dt <- data.table(col1 = c("a,b,c", "d,e,f"), col2 = c("x,y,z", "1,2,3"))
#' identifierCols <- c("col1", "col2")
#' updatedDt <- updateIdentifierColumns(dt, identifierCols)
#' @export
convertIdentifierColumns <- function(dt, identifierCols) {
  for (col in identifierCols) {
    dt[[col]] <- as.character(dt[[col]])
    if (any(grepl(",", dt[[col]]))) {
      warning(paste("Warning: Column", col, "commas were replace by _"))
    }
    dt[[col]] <- gsub(",", "_", dt[[col]])
  }
  return(dt)
}

