#' Load Scenario Time Profiles
#'
#' This function aggregates simulated time profiles for different scenarios based on the provided results and output paths.
#'
#' @param simulatedResults A list containing simulation results for each scenario.
#' @param outputPathsPerScenario A named list of output paths for each scenario.
#' @template projectConfig
#' @param aggregationFun A function to aggregate simulation data.
#'
#' @return A data.table containing the aggregated simulated time profiles for all scenarios.
#' @export
loadScenarioTimeProfiles <- function(projectConfiguration, simulatedResults, outputPathsPerScenario, aggregationFun) {
  dtSimulated <- data.table()
  for (scenarioName in names(outputPathsPerScenario)) {

    individualMatch <- getIndividualMatchForScenario(projectConfiguration = projectConfiguration,
                                  scenario = scenarioName)

    dtSimulated <- rbind(
      dtSimulated,
      getSimulatedTimeprofile(simulatedResult = simulatedResults[[scenarioName]],
                              outputPaths = outputPathsPerScenario[[scenarioName]],
                              aggregationFun = aggregationFun,
                              individualMatch = individualMatch) %>%
        dplyr::mutate(Scenario = scenarioName)
    )
  }
  return(dtSimulated)
}

#' Get Unit Conversion Data Table
#'
#' This function creates a data table for unit conversion based on simulated data and output specifications.
#'
#' @param dtSimulated A data.table containing simulated results.
#' @param dtOutputs A data.table containing output specifications.
#' @return A data.table with unit conversion factors and unique paths.
#' @export
getUnitConversionDT <- function(dtSimulated, dtOutputs) {
  dtUnit <- dtSimulated %>%
    dplyr::select("paths", "dimension", "yUnit", "molWeight") %>%
    unique() %>%
    merge(
      dtOutputs %>%
        dplyr::select("outputPathId", "DisplayUnit", "OutputPath"),
      by.x = "paths",
      by.y = "OutputPath"
    )

  dtUnit[, unitFactor := apply(dtUnit, 1, function(row) {
    toUnit(
      quantityOrDimension = row["dimension"],
      values = 1,
      sourceUnit = row["yUnit"],
      targetUnit = row["DisplayUnit"],
      molWeight = as.numeric(row["molWeight"]),
      molWeightUnit = "g/mol"
    )
  })]

  return(dtUnit)
}

#' Get Application Times
#'
#' This helper function prepares the timerange filter for application times based on output paths and simulation results.
#'
#' @param outputPathsPerScenario A named list of output paths for each scenario.
#' @param simulatedResult The results of the simulation.
#' @return A list containing the start and end times of applications for each scenario.
#' @export
getApplicationTimes <- function(outputPathsPerScenario, simulatedResults) {
  applicationTimes <- list()

  for (scenarioName in names(outputPathsPerScenario)) {
    applicationStartTimes <- lapply(outputPathsPerScenario[[scenarioName]], function(pts) {
      lapply(
        simulatedResults[[scenarioName]]$simulation$allApplicationsFor(pts),
        function(x) {
          x$startTime$value
        }
      ) %>%
        unlist()
    }) %>%
      unlist() %>%
      unique() %>%
      sort()

    applicationTimes[[scenarioName]] <-
      list(
        startOfFirstApplication = applicationStartTimes[1],
        startOfLastApplication = utils::tail(applicationStartTimes, 1),
        endOfFirstApplication =
          ifelse(length(applicationStartTimes) > 1, applicationStartTimes[2], Inf) # nolint: line_length
      )
  }
  return(applicationTimes)
}

#' Get Simulated Time Profile
#'
#' This helper function processes simulation results into a data table format for time profiles.
#'
#' @param simulatedResult The simulation results to be processed.
#' @param outputPaths A vector of output paths to be included in the time profile.
#' @param individualMatch data.table with matches simulated individual id with individual id of observed data,
#'    is only fille for individual poulations otherwise NULL
#' @param aggregationFun A function to aggregate the simulation data if necessary.
#'
#' @return A data.table with the processed time profile data.
#' @keywords internal
getSimulatedTimeprofile <- function(simulatedResult, outputPaths, aggregationFun,individualMatch) {

  dt <- ospsuite::simulationResultsToDataFrame(
    simulationResults = simulatedResult$results,
    quantitiesOrPaths = outputPaths
  ) %>%
    data.table::setDT()

  # Rename columns
  data.table::setnames(
    x = dt,
    old = c("Time", "simulationValues", "unit"),
    new = c("xValues", "yValues", "yUnit")
  )

  # Aggregate if needed
  if (!is.null(individualMatch)){

    dt <- dt %>%
      dplyr::select(c( "IndividualId","xValues", "yValues", "paths", "dimension", "yUnit", "molWeight")) %>%
      merge(individualMatch, by = 'IndividualId') %>%
      dplyr::mutate(IndividualId = NULL) %>%
      data.table::setnames(old = 'ObservedIndividualId', new = 'individualId') %>%
      dplyr::mutate(dataClass = DATACLASS$tpIndPop)

  } else if (dplyr::n_distinct(dt$IndividualId) > 1) {
    dt <- performAggregation(
      dataToAggregate = dt,
      aggregationFun = aggregationFun,
      aggrCriteria = c( "xValues", "paths", "dimension", "yUnit", "molWeight")
    ) %>%
      dplyr::mutate(dataClass = DATACLASS$tpAggregated)
  } else {
    dt <- dt %>%
      dplyr::select(c( "xValues", "yValues", "paths", "dimension", "yUnit", "molWeight")) %>%
      dplyr::mutate(dataClass = DATACLASS$tpIndividual)
  }

  dt <- dt %>%
    dplyr::mutate(dataType = "simulated")


  return(dt)
}

#' Convert Y Unit
#'
#' This function converts the y-values of a time profile based on unit conversion factors.
#'
#' @param timeprofile A data.table containing time profile data.
#' @param dtUnit A data.table with unit conversion factors.
#' @return A data.table with the y-values converted to the target unit.
#' @export
convertYunit <- function(timeprofile, dtUnit) {
  identifier <- intersect(names(dtUnit),names(timeprofile))
  timeprofile <- timeprofile %>%
    merge(dtUnit %>% dplyr::select(unique(c(identifier, 'outputPathId','unitFactor','DisplayUnit'))),
          by = identifier
    )

  columnsToScale <- intersect(
    c("yValues", "lloq", "yMin", "yMax"),
    names(timeprofile)
  )
  yErrorType <- timeprofile$yErrorType[1]
  if (!is.null(yErrorType) &&
      !is.na(yErrorType) && # nolint: line_length
      yErrorType == ospsuite::DataErrorType$ArithmeticStdDev) {
    columnsToScale <- c(columnsToScale, yErrorValues)
  }
  timeprofile[, (columnsToScale) := lapply(.SD, function(x) x * unitFactor), .SDcols = columnsToScale]

  # delete columns not needed any more
  timeprofile <-  timeprofile %>%
    dplyr::select(-any_of(c("paths","dimension", "molWeight","unitFactor","yUnit")))

  data.table::setnames(timeprofile,old = 'DisplayUnit',new = 'yUnit')


  return(timeprofile)
}

#' Convert and Shift Time Units
#'
#' This function converts time values to a target unit and applies an optional time offset.
#'
#' @param timeprofile A data.table containing time profile data.
#' @param targetTimeUnit The target time unit for conversion.
#' @param timeOffset An optional time offset to be applied after conversion (default is 0).
#' @return A data.table with the time values converted and shifted.
#' @export
convertAndShiftTimeUnits <- function(timeprofile, targetTimeUnit, timeOffset = 0) {
  timeprofile[, xValues := toUnit(
    quantityOrDimension = "Time",
    values = as.double(xValues),
    targetUnit = targetTimeUnit
  )]
  timeprofile[, xUnit := targetTimeUnit]

  timeprofile[, xValues := xValues - timeOffset]

  return(timeprofile)
}


#' Filter Time Profile by Individual IDs
#'
#' This function filters a given time profile data frame to include only the rows
#' corresponding to specified individual IDs. If the individual list is not provided
#' othe the timeprofile has no column "individualId" the original time profile is returned.
#'
#' @param timeprofile A data frame containing time profile data with an `individualId` column.
#' @param individualList A character string of individual IDs to filter by. If the string
#'                       contains '*', all individuals will be included.
#'
#' @return A data frame containing only the rows from `timeprofile` that match the specified
#'         individual IDs. If no IDs are provided, the original `timeprofile` is returned.
#'
#' @export
filterIndividualID = function(timeprofile,individualList){
  if (!is.na(individualList) & 'individualId' %in% names(timeprofile)){
    individualIds = gsub("[()]", "", splitInputs(individualList))
    if (any(individualIds != '*'))
      timeprofile <- timeprofile[individualId %in% individualIds,]
  }
  return(timeprofile)
}
