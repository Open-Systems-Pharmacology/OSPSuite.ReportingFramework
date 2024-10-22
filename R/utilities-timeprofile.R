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

  dtScenarios <- getScenarioDefinitions(projectConfiguration)
  for (scenarioName in names(outputPathsPerScenario)) {

    individualMatch <- NULL
    if ("ObservedIndividualId" %in% simulatedResults[[scenarioName]]$population$allCovariateNames){
      individualMatch <-
        data.table(individualId = simulatedResults[[scenarioName]]$population$allIndividualIds,
                   observedIndividualId = simulatedResults[[scenarioName]]$population$getCovariateValues('ObservedIndividualId'))
    }

    dtSimulated <- rbind(
      dtSimulated,
      getSimulatedTimeprofile(
        simulatedResult = simulatedResults[[scenarioName]],
        outputPaths = outputPathsPerScenario[[scenarioName]],
        aggregationFun = aggregationFun,
        individualMatch = individualMatch
      ) %>%
        dplyr::mutate(scenario = scenarioName)
    )
  }
  return(dtSimulated)
}


#' Prepare Data for Matching
#'
#' This function prepares observed data for matching with simulation results based on project configuration and scenario definitions.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details, including data groups and output paths.
#' @param dataObserved A data.table containing observed data. It must include columns for 'group', 'yValues', and 'yUnit'.
#' @param scenarioList A list of scenarios, each containing simulation details relevant to the observed data.
#'
#' @return A data.table containing the prepared observed data, with adjusted values and units for matching with simulation results.
#'
#' @details The function checks for valid scenarios, merges data tables, calculates conversion factors, and ensures that the data class matches the expected format.
#'
#' @export
prepareDataForMatch <- function(projectConfiguration, dataObserved, scenarioList) {

  # make sure not to change dataObserved outside function
  dataObservedForMatch <- data.table::copy(dataObserved)

  # Retrieve data groups and output paths
  dtDataGroups <- getDataGroups(projectConfiguration)
  dtOutputs <- getOutputPathIds(projectConfiguration)
  scenarioNames <- names(scenarioList)

  # Check if all scenario names are in the default scenarios
  if (!all(scenarioNames %in% dtDataGroups$defaultScenario)) {
    stop(
      paste(
        'There are scenarios which are not selected as "DefaultScenario" in sheet "DataGroups" "Plots.xlsx".',
        'This is mandatory to connect data and simulations:',
        paste(setdiff(scenarioNames, dtDataGroups$defaultScenario), collapse = ', ')
      )
    )
  }

  # Clean up observed data
  if ('scenario' %in% names(dataObservedForMatch)) dataObservedForMatch[, scenario := NULL]
  if ('outputPath' %in% names(dataObservedForMatch)) dataObservedForMatch[, outputPath := NULL]

  # Merge data tables
  dataObservedForMatch <- dataObservedForMatch %>%
    merge(dtDataGroups[, c('group', 'defaultScenario')], by = 'group') %>%
    data.table::setnames(old = 'defaultScenario', 'scenario') %>%
    merge(dtOutputs[, c('outputPathId', 'outputPath')], by = 'outputPathId')

  dataObservedForMatch <- calculateUnitFactors(dataObservedForMatch,dtOutputs,scenarioList)

  # Check data class
  if (!all(dataObservedForMatch$dataClass == DATACLASS$tpIndividual)) {
    stop(
      paste(
        'Please select only scenarios which are matched as "DefaultScenarios" in sheet "DataGroups" "Plots.xlsx" to individual data.',
        'Check dataGroup',
        paste(unique(dataObservedForMatch[dataClass != DATACLASS$tpIndividual]$group), collapse = ', ')
      )
    )
  }

  return(dataObservedForMatch)
}

#' Calculate Unit Factors and Adjust Observed Data
#'
#' This function calculates unit conversion factors for observed data and adjusts the observed data accordingly.
#'
#' @param dataObserved A data.table containing observed data. It must include columns for 'scenario', 'outputPathId', 'yUnit', and 'yValues'.
#' @param dtOutputs A data.table containing output path information, including 'outputPathId' and 'displayUnit'.
#' @param scenarioList A list of scenarios, each containing simulation details relevant to the observed data.
#'
#' @return A data.table containing the updated observed data with adjusted yValues, unit factors, and units.
#'
#' @details The function merges unit conversion factors into the observed data, adjusts the yValues based on the calculated data factors,
#' and computes the unit factor for time.
#'
#' @keywords internal
calculateUnitFactors <- function(dataObserved,dtOutputs,scenarioList){
  # Get unit conversion factors
  dtUnit <- unique(dataObserved[, c('scenario', 'outputPathId', 'yUnit')]) %>%
    merge(dtOutputs, by = 'outputPathId')

  # Calculate unit factor for x
  dtUnit[, unitFactorX := ospsuite::toUnit(quantityOrDimension = 'Time', values = 1, targetUnit = dataObserved$xUnit[1])]

  # Initialize conversion factors
  dtUnit[, dataFactor := NA_real_]
  dtUnit[, unitFactorY := NA_real_]
  dtUnit[, endTimeSimulation := NA_real_]

  # Calculate conversion factors
  for (iRow in seq_len(nrow(dtUnit))) {
    sim <- scenarioList[[dtUnit$scenario[iRow]]]$simulation
    quantity <- ospsuite::getQuantity(path = dtUnit$outputPath[iRow], container = sim)

    dtUnit$dataFactor[iRow] <- ospsuite::toUnit(
      quantityOrDimension = quantity$dimension,
      values = 1,
      targetUnit = dtUnit$displayUnit[iRow],
      sourceUnit = dtUnit$yUnit[iRow],
      molWeight = sim$molWeightFor(dtUnit$outputPath[iRow])
    )

    dtUnit$unitFactorY[iRow] <- ospsuite::toUnit(
      quantityOrDimension = quantity$dimension,
      values = 1,
      targetUnit = dtUnit$displayUnit[iRow],
      sourceUnit = quantity$unit,
      molWeight = sim$molWeightFor(dtUnit$outputPath[iRow])
    )

    dtUnit[, endTimeSimulation := sim$outputSchema$endTime * unitFactorX]
  }

  # Merge conversion factors back to observed data
  dataObserved <- dataObserved %>%
    merge(dtUnit[,c('scenario', 'outputPathId', 'unitFactorX', 'unitFactorY', 'dataFactor', 'displayUnit','endTimeSimulation')],
          by = c('scenario', 'outputPathId'))

  # Adjust yValues and units
  dataObserved[, yValues := yValues * dataFactor]
  dataObserved[, dataFactor := NULL]
  dataObserved[, yUnit := displayUnit]
  dataObserved[, displayUnit := NULL]

  if (any(dataObserved$xValues < 0)){
    warning('data with time < 0 is outside simulation range will be ignored')
    dataObserved <- dataObserved[xValues >= 0]
  }
  if (any(dataObserved$xValues > dataObserved$endTimeSimulation)){
    writeTableToLog(dataObserved[xValues > endTimeSimulation] %>%
                      dplyr::select(any_of(c(getColumnsForColumnType(dataObservedForMatch,'identifier'),
                                             'xValues','endTimeSimulation','timeUnit'))))
    warning('data with time outside simulation range will be ignored')
    dataObserved <- dataObserved[xValues <= endTimeSimulation]
  }
  dataObserved[,endTimeSimulation := NULL]


  return(dataObserved)
}

#' Get Predictions for Scenarios
#'
#' This function generates predictions for a set of scenarios based on observed data.
#'
#' @param scenarioResults A list of scenario results, each containing population and covariate information.
#' @param dataObserved A data.table containing observed data, which must include the required columns:
#'        'scenario', 'outputPathId', 'yValues', and 'yUnit'.
#'        The unit factors (`unitFactorX` and `unitFactorY`) are necessary for proper scaling of the predicted values
#'        and can be generated using the `prepareDataForMatch` function prior to calling this function.
#' @param aggregationFun A function for aggregation (optional). If provided, it will be used to aggregate the simulated results.
#' @param identifier A character vector of identifiers for matching, defaults to c("outputPath", "individualId").
#'
#' @return A data.table containing the predicted values for each scenario, along with the scenario name and other relevant identifiers.
#'
#' @details The function validates the input names, loops through each scenario to generate predictions, and combines all results into a single data.table.
#' It also handles individual matching if 'ObservedIndividualId' is present in the scenario results.
#'
#' @export
getPredictionsForScenarios <- function(scenarioResults,
                                       dataObservedForMatch,
                                       aggregationFun = NULL,
                                       identifier = c("outputPath", "individualId")) {

  # Validate input names
  checkmate::assertNames(names(dataObservedForMatch), must.include = c('scenario', 'unitFactorX','unitFactorY',identifier))

  # Initialize a list to store results for each scenario
  resultsList <- list()

  # Loop through each scenario
  for (scenarioName in names(scenarioResults)) {
    scenarioResult <- scenarioResults[[scenarioName]]

    individualMatch <- NULL
    if ("ObservedIndividualId" %in% scenarioResult$population$allCovariateNames) {
      individualMatch <- data.table(
        individualId = scenarioResult$population$allIndividualIds,
        observedIndividualId = scenarioResult$population$getCovariateValues('ObservedIndividualId')
      )
    }

    # Get simulated time profile
    dtSimulated <- getSimulatedTimeprofile(
      simulatedResult = scenarioResult,
      outputPaths = unique(dataObservedForMatch$outputPath),
      aggregationFun = aggregationFun,
      individualMatch = individualMatch
    ) %>%
      dplyr::mutate(scenario = scenarioName) %>%
      data.table::setnames('paths', 'outputPath') %>%
      merge( dataObservedForMatch[scenario == scenarioName,] %>%
               dplyr::select(all_of(c(identifier,'unitFactorX','unitFactorY'))) %>%
               unique(),
             by = identifier) %>%
      .[,xValues := xValues*unitFactorX] %>%
      .[,yValues := yValues*unitFactorY]

    # Add predicted values and store in the results list
    resultsList[[scenarioName]] <- addPredictedValues(
      dtObserved = dataObservedForMatch[scenario == scenarioName],
      dtSimulated = dtSimulated,
      identifier = identifier
    ) %>%
      dplyr::mutate(scenarioName = scenarioName)
  }

  # Combine all results into a single data.table
  dtResult <- rbindlist(resultsList, use.names = TRUE, fill = TRUE)

  return(dtResult)
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
  # avoid warning for global variable
  unitFactor <- NULL
  dtUnit <- dtSimulated %>%
    dplyr::select("paths", "dimension", "yUnit", "molWeight") %>%
    unique() %>%
    merge(
      dtOutputs %>%
        dplyr::select("outputPathId", "displayUnit", "outputPath"),
      by.x = "paths",
      by.y = "outputPath"
    )

  dtUnit[, unitFactor := apply(dtUnit, 1, function(row) {
    ospsuite::toUnit(
      quantityOrDimension = row["dimension"],
      values = 1,
      sourceUnit = row["yUnit"],
      targetUnit = row["displayUnit"],
      molWeight = as.numeric(row["molWeight"]),
      molWeightUnit = "g/mol"
    )
  })]

  return(dtUnit)
}

#' Get Application Times
#'
#' This helper function prepares the time range filter for application times based on output paths and simulation results.
#'
#' @param outputPathsPerScenario A named list of output paths for each scenario.
#' @param simulatedResults List with simulation results
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
#'    is only filled for individual populations otherwise NULL
#' @param aggregationFun A function to aggregate the simulation data if necessary.
#'
#' @return A data.table with the processed time profile data.
#' @keywords internal
getSimulatedTimeprofile <- function(simulatedResult, outputPaths, aggregationFun, individualMatch) {
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
  if (!is.null(individualMatch)) {
    dt <- dt %>%
      dplyr::select(c("IndividualId", "xValues", "yValues", "paths", "dimension", "yUnit", "molWeight")) %>%
      merge(individualMatch, by.x = "IndividualId",by.y = "individualId") %>%
      dplyr::mutate(IndividualId = NULL) %>%
      data.table::setnames(old = "observedIndividualId", new = "individualId") %>%
      dplyr::mutate(dataClass = DATACLASS$tpTwinPop)
  } else if (dplyr::n_distinct(dt$IndividualId) > 1) {
    dt <- performAggregation(
      dataToAggregate = dt,
      aggregationFun = aggregationFun,
      aggrCriteria = c("xValues", "paths", "dimension", "yUnit", "molWeight")
    ) %>%
      dplyr::mutate(dataClass = DATACLASS$tpAggregated)
  } else {
    dt <- dt %>%
      dplyr::select(c("xValues", "yValues", "paths", "dimension", "yUnit", "molWeight")) %>%
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
  # Initialize variables used for data.tables
  yErrorValues <- unitFactor <- NULL

  identifier <- intersect(names(dtUnit), names(timeprofile))
  timeprofile <- timeprofile %>%
    merge(dtUnit %>% dplyr::select(unique(c(identifier, "outputPathId", "unitFactor", "displayUnit"))),
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
  timeprofile <- timeprofile %>%
    dplyr::select(-dplyr::any_of(c("paths", "dimension", "molWeight", "unitFactor", "yUnit")))

  data.table::setnames(timeprofile, old = "displayUnit", new = "yUnit")


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
  # avoid warnings during check
  xValues <- xUnit <- NULL

  sourceTimeUnit <- unique(timeprofile$xUnit)

  timeprofile[, xValues := ospsuite::toUnit(
    quantityOrDimension = "Time",
    sourceUnit = sourceTimeUnit,
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
#' or the time profile has no column "individualId" the original time profile is returned.
#'
#' @param timeprofile A data frame containing time profile data with an `individualId` column.
#' @param individualList A character string of individual IDs to filter by. If the string
#'                       contains '*', all individuals will be included.
#'
#' @return A data frame containing only the rows from `timeprofile` that match the specified
#'         individual IDs. If no IDs are provided, the original `timeprofile` is returned.
#'
#' @export
filterIndividualID <- function(timeprofile, individualList) {
  # Initialize variables used for data.tables
  individualId <- NULL

  if (!is.na(individualList) & "individualId" %in% names(timeprofile)) {
    individualIds <- gsub("[()]", "", splitInputs(individualList))
    if (any(individualIds != "*")) {
      timeprofile <- timeprofile[individualId %in% individualIds, ]
    }
  }
  return(timeprofile)
}
