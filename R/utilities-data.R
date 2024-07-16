#' Read data by dictionary
#'
#' This function reads and processes data based on the provided project configuration.
#'
#' @param projectConfiguration The project configuration data
#' @return Processed data based on the dictionary
#' @export
readObservedDataByDictionary <- function(projectConfiguration) {
  checkmate::assertFileExists(projectConfiguration$dataImporterConfigurationFile)

  # initialize variables used in data.table to avoid message during package build
  weighting <- NULL

  logCatch({
    dataList <- esqlabsR::readExcel(projectConfiguration$dataImporterConfigurationFile, sheet = "DataFiles")
    # delete description line
    dataList <- dataList[-1, ]
    dataList <- dataList[rowSums(is.na(dataList)) < ncol(dataList), ]

    data <- data.table()
    dict <- list()
    for (d in split(dataList, nrow(dataList))) {
      tmpData <- data.table::fread(fs::path_abs(
        start = projectConfiguration$projectConfigurationDirPath,
        path = d$DataFile
      ))

      tmpdict <- readDataDictionary(
        dictionaryFile = projectConfiguration$dataImporterConfigurationFile,
        sheet = d$Dictionary,
        data = tmpData
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
          setNames(tmpdict$targetColumn)
      )
    }

    # set weighting variable
    data[, weighting := 1]

    # add dictionary as attributes
    dict[["timeUnit"]] <- "timeprofile"
    for (dc in names(dict)) {
      data.table::setattr(data[[dc]], "columnType", dict[[dc]])
    }

    validateObservedData(data, stopIfValidationFails = FALSE)
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
#' @param data The data to be used with the dictionary
#' @return The data dictionary
readDataDictionary <- function(dictionaryFile, sheet, data) {
  # initialize variables used fo data.tables
  sourceColumn <- NULL

  dict <- esqlabsR::readExcel(dictionaryFile, sheet = sheet) %>%
    data.table::setDT()
  dict <- dict[-1, ]
  dict <- dict[rowSums(is.na(dict)) < ncol(dict), ]

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
  dictFilters <- dict[!is.na(filter)]

  for (myFilter in split(dictFilters, nrow(dictFilters))) {
    data[
      eval(parse(text = myFilter$filter)),
      (myFilter$targetColumn) := eval(parse(text = myFilter$filterValue))
    ]
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

  # add time unit and convert biometrics to appropriate columns
  data[, timeUnit := dict[targetColumn == "time"]$sourceUnit]

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

  return(data)
}
