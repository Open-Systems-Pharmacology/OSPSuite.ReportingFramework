#' Setup Virtual Twin Population Configuration
#'
#' This function sets up a configuration sheet for virtual twin populations based on the provided project configuration and observed data.
#'
#' @param projectConfiguration A list containing project configuration details, including file paths for populations and scenarios.
#' @param dataObserved A data object containing observed data, which may be of class "DataCombined".
#' @param groups A character vector of group names to filter the virtual twin population. Defaults to NULL.
#'
#' @return Returns NULL invisibly if no groups are available for virtual twin population creation. Modifies the workbook specified in projectConfiguration.
#' @export
setupVirtualTwinPopConfig <- function(projectConfiguration, dataObserved, groups = NULL) {
  # avoid warning for global variable
  populationName <- dataGroups <- group <- NULL

  checkmate::assertCharacter(groups, any.missing = FALSE, null.ok = TRUE)

  # Check if dataObserved is of class "DataCombined" and convert it if necessary
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  # Load configuration table for virtual twin population
  wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

  # Check if the 'VirtualTwinPopulation' sheet exists
  if (!("VirtualTwinPopulation" %in% wb$sheet_names)) {
    dtTwinPops <- xlsxReadData(projectConfiguration$scenariosFile, sheetName = "Scenarios") %>%
      data.table::setnames("populationId", "populationName") %>%
      dplyr::mutate("dataGroups" = "") %>%
      dplyr::select(c("populationName", "dataGroups", "individualId", "modelParameterSheets", "applicationProtocol")) %>%
      dplyr::filter(FALSE)
  } else {
    dtTwinPops <- xlsxReadData(wb, "VirtualTwinPopulation")
  }

  if (is.null(groups)) groups <- getIndividualDataGroups(dataObserved, groups)
  # Remove any groups that are already in dtTwinPops
  groups <- setdiff(groups, unique(splitInputs(dtTwinPops$dataGroups)))

  # Check if any groups are available for virtual twin population creation
  if (length(groups) == 0) {
    message("No groups available for virtual twin population creation")
    return(NULL)
  }

  # Create new virtual twin population data
  dtTwinPopsNew <- dataObserved[group %in% groups, c("group", "individualId")] %>%
    unique() %>%
    .[, .(dataGroups = paste(group, collapse = ", ")), by = "individualId"] %>%
    .[, populationName := gsub(", ", "_", dataGroups)]

  message("add virtual twin population configuration in Population configuration file:")
  writeTableToLog(dtTwinPopsNew[, .N, by = "populationName"])

  # Combine the existing and new virtual twin population data
  dtTwinPops <- rbind(dtTwinPops, dtTwinPopsNew, fill = TRUE)

  # Write data to the workbook
  if (!("VirtualTwinPopulation" %in% wb$sheet_names)) {
    xlsxAddSheet(wb = wb, sheetName = "VirtualTwinPopulation", dt = dtTwinPops)
  } else {
    xlsxWriteData(wb = wb, sheetName = "VirtualTwinPopulation", dt = dtTwinPops)
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$populationsFile, overwrite = TRUE)

  return(invisible())
}


#' Generate Virtual Twin Population
#'
#' This function generates virtual twin populations based on the provided model and project configuration.
#'
#' @param projectConfiguration A list containing project configuration details, including file paths for populations and scenarios.
#' @param modelFile A string representing the name of the model file to be loaded. This file is used for unit conversion
#' @param populationNames a character vector defining the population which should be exported. If NULL (default) all will be exported
#' @param overwrite A logical indicating whether to overwrite existing files. Defaults to FALSE
#'
#' @export
#'
exportVirtualTwinPopulations <- function(projectConfiguration, modelFile, overwrite = FALSE, populationNames = NULL) {
  # Initialize variables used for data.tables
  populationName <- individualId <- NULL
  checkmate::assertCharacter(populationNames, any.missing = FALSE, null.ok = TRUE)

  dtTwinPops <- xlsxReadData(
    wb = projectConfiguration$populationsFile,
    sheetName = "VirtualTwinPopulation",
    emptyAsNA = FALSE
  )

  # Check if overwrite is FALSE and filter for existing files
  if (!overwrite) {
    existingFiles <- list.files(projectConfiguration$populationsFolder, pattern = "*.csv", full.names = TRUE)
    existingPopulationNames <- sub("\\.csv$", "", basename(existingFiles))

    # Filter dtTwinPops for populations that do not exist
    dtTwinPops <- dtTwinPops[!populationName %in% existingPopulationNames]
  }
  if (!is.null(populationNames) & nrow(dtTwinPops) > 0) {
    dtTwinPops <- dtTwinPops[dtTwinPops$populationName %in% populationNames]
  }
  # If no populations left to generate, return with a message
  if (nrow(dtTwinPops) == 0) {
    message("No new virtual twin populations to generate; all files already exist.")
    return(invisible())
  }

  sim <- ospsuite::loadSimulation(file.path(projectConfiguration$modelFolder, modelFile))

  params <- .readParameterSheetList(projectConfiguration, dtTwinPops, sim)

  dtIndividualBiometrics <- xlsxReadData(wb = projectConfiguration$individualsFile, sheetName = "IndividualBiometrics")
  dtIndividualBiometrics <- dtIndividualBiometrics[individualId %in% dtTwinPops$individualId]


  .generatePopulationFiles(dtTwinPops, params, dtIndividualBiometrics, projectConfiguration, sim)

  return(invisible())
}

#' Get Individual Match for a Scenario
#'
#' This function retrieves the individual match data for a specified scenario
#' from a project configuration. It checks if the scenario is a population
#' scenario with a static population file containing the column ObservedIndividualId
#' and reads the IndividualId of the simulated results and the ObservedIndividualId
#' of the observed data.
#'
#' @param projectConfiguration A list containing the project configuration,
#' including the path to the populations folder.
#' @param scenario A string specifying the name of the scenario for which
#' the individual match data is to be retrieved.
#' @param dtScenarios A data table containing scenario details.
#'
#' @return A data.table containing the 'IndividualId' and 'ObservedIndividualId'
#' if the population scenario is a virtual twin population; otherwise, returns NULL.
#'
#' @export
getIndividualMatchForScenario <- function(projectConfiguration,
                                          scenario,
                                          dtScenarios) {
  # avoid warnings for global variables
  scenario_name <- observedIndividualId <- NULL # nolint object_name_linter

  dtScenarioRow <- dtScenarios[scenario_name == scenario]

  # check if is is a population scenario with a static population file
  if (is.na(dtScenarioRow$populationId) ||
      is.na(dtScenarioRow$readPopulationFromCSV) || # nolint indentation_linter
      dtScenarioRow$readPopulationFromCSV == 0) {
    return(NULL)
  }

  # read static population file
  filename <- file.path(projectConfiguration$populationsFolder, paste0(dtScenarioRow$populationId, ".csv"))
  checkmate::assertFile(filename)
  poptable <- data.table::fread(filename)

  # check if ths is an virtual twin Population with column ObservedIndividualId
  if (!("ObservedIndividualId" %in% names(poptable))) {
    return(NULL)
  }

  poptable[, ObservedIndividualId := as.character(ObservedIndividualId)]

  return(poptable %>% dplyr::select("IndividualId", "ObservedIndividualId"))
}

#' Export Random Populations
#'
#' This function exports random populations based on demographic ranges from a
#' specified Excel file. It can filter populations to export based on existing
#' files in a specified folder and optionally overwrite those files.
#'
#' @param projectConfiguration A object of class ProjectConfiguration containing configuration settings, including
#'                             the path to the populations file and the folder for
#'                             output files.
#' @param populationNames A character vector of population names to export. If NULL,
#'                        all populations in the demographics sheet will be considered.
#' @param overwrite A logical value indicating whether to overwrite existing population
#'                  files. Default is FALSE.
#'
#' @return NULL (invisible), or an informative message if no new populations are
#'         available for export.
#'
#' @export
exportRandomPopulations <- function(projectConfiguration, populationNames = NULL, overwrite = FALSE) {
  # add virtual population with in biometric ranges of observed data
  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")

  if (is.null(populationNames)) populationNames <- dtPops$populationName

  # Check if overwrite is FALSE and filter for existing files
  if (!overwrite) {
    existingFiles <- list.files(projectConfiguration$populationsFolder, pattern = "*.csv", full.names = TRUE)
    existingPopulationNames <- sub("\\.csv$", "", basename(existingFiles))

    # Filter dtTwinPops for populations that do not exist
    populationNames <- setdiff(populationNames, existingPopulationNames)
  }
  if (!is.null(populationNames) & nrow(dtPops) > 0) {
    dtPops <- dtPops[dtPops$populationName %in% populationNames]
  }
  # If no populations left to generate, return with a message
  if (nrow(dtPops) == 0) {
    message("No new virtual populations to generate; all files already exist.")
    return(invisible())
  }

  lapply(
    split(dtPops, by = "populationName"),
    function(dPop) {
      popCharacteristics <- esqlabsR::readPopulationCharacteristicsFromXLS(
        XLSpath = projectConfiguration$populationsFile,
        populationName = dPop$populationName,
        sheet = "Demographics"
      )
      population <- ospsuite::createPopulation(populationCharacteristics = popCharacteristics)

      if (dPop$populationName %in% openxlsx::getSheetNames(projectConfiguration$populationsFile)) {
        extendPopulationFromXLS_RF(population, projectConfiguration$populationsFile, sheet = dPop$populationName)
      }

      ospsuite::exportPopulationToCSV(
        population = population$population,
        filePath = file.path(projectConfiguration$populationsFolder, paste0(dPop$populationName, ".csv"))
      )
      return(invisible())
    }
  )

  return(invisible())
}


#' Update Exported Population
#'
#' This function updates an existing exported population by loading data from a
#' specified source population file and extending it with additional data from
#' an Excel sheet. It checks for the existence of the target population file and
#' can optionally overwrite existing files.
#'
#' @param projectConfiguration A list containing configuration settings, including
#'                             the path to the populations folder and the populations file.
#' @param sourcePopulation A character string representing the name of the source
#'                         population to load.
#' @param targetPopulation A character string representing the name of the target
#'                         population to be updated.
#' @param sheetName A character string specifying the name of the sheet in the Excel
#'               file from which to extend the population data.
#' @param overwrite A logical value indicating whether to overwrite the existing
#'                  target population file. Default is FALSE.
#'
#' @return NULL (invisible), or a warning if the target population already exists
#'         and overwrite is FALSE.
#'
#' @export
updateExportedPopulation <- function(projectConfiguration, sourcePopulation, targetPopulation, sheetName, overwrite = FALSE) {
  sourcepopFilename <- file.path(projectConfiguration$populationsFolder, paste0(sourcePopulation, ".csv"))

  checkmate::assertFileExists(sourcepopFilename)

  # Check if overwrite is FALSE and filter for existing files
  if (!overwrite) {
    existingFiles <- list.files(projectConfiguration$populationsFolder, pattern = "*.csv", full.names = TRUE)
    existingPopulationNames <- sub("\\.csv$", "", basename(existingFiles))

    if (targetPopulation %in% existingPopulationNames) {
      warning(paste(targetPopulation, "already exists, nothing is done. Do you want to set overwrite to TRUE?"))
      return(invisible())
    }
  }

  population <- ospsuite::loadPopulation(sourcepopFilename)
  extendPopulationFromXLS_RF(
    population = population,
    XLSpath = projectConfiguration$populationsFile, sheet = sheetName
  )
  ospsuite::exportPopulationToCSV(
    population = population,
    filePath = file.path(projectConfiguration$populationsFolder, paste0(targetPopulation, ".csv"))
  )

  return(invisible())
}

#' Set Custom Parameters to Population
#'
#' This function updates the parameter values of a population based on custom parameters defined in a scenario.
#' It first checks if the scenario is of type "Population" and whether custom parameters are available.
#'
#' @param scenario An object of class `Scenario` containing the following components:
#'   - `scenarioType`: A character string indicating the type of scenario.
#'   - `finalCustomParams`: A list with custom parameters.
#'   - `population`: An object representing the population, which includes a method to set parameter values.
#'   - `simulation`: An object containing simulation details, used to retrieve parameter dimensions.
#'
#' @details
#' The function filters the custom parameters to include only those that exist in the population's parameter paths.
#' It calculates the base values for these parameters and sets them for the entire population if applicable.
#'
#' @return The updated `scenario` object, with the population's parameters set accordingly. If the scenario type is not "Population" or if there are no custom parameters, the original scenario is returned unchanged.
#'
#' @export
setCustomParamsToPopulation <- function(scenario) {
  # avoid warning for global variable
  paths <- dimension <- values <- NULL

  checkmate::assertClass(scenario, classes = "Scenario")
  if (scenario$scenarioType != "Population" ||
      is.null(scenario$finalCustomParams$paths)) { # nolint indentation_linter
    return(scenario)
  }


  dtCustomParams <- data.table::as.data.table(scenario$finalCustomParams)
  dtCustomParams <- dtCustomParams[paths %in% scenario$population$allParameterPaths]
  if (nrow(dtCustomParams) > 0) {
    dtCustomParams[, `:=`(
      dimension = ospsuite::getParameter(paths, container = scenario$simulation)$dimension
    ),
    by = "paths"
    ]
    dtCustomParams[, `:=`(
      baseValue = ospsuite::toBaseUnit(quantityOrDimension = dimension, values = values, unit = units)
    ),
    by = "paths"
    ]

    for (dp in split(dtCustomParams, by = "paths")) {
      scenario$population$setParameterValues(
        parameterOrPath = dp$paths,
        values = rep(dp$baseValue, scenario$population$count)
      )
    }
  }
  return(scenario)
}


#' Read Parameter Sheet List
#'
#' This function reads parameters from specified sheets in an Excel file.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param dtTwinPops A data.table containing virtual twin population data.
#' @param sim A simulation object.
#'
#' @return A list of parameters for the specified sheets.
#' @keywords internal
.readParameterSheetList <- function(projectConfiguration, dtTwinPops, sim) {
  params <- mapply(
    function(sheet, file) {
      .getAllParameterForSheets(
        projectConfiguration = projectConfiguration,
        sheets = .cleanUpSheetList(dtTwinPops[[sheet]]),
        paramsXLSpath = file,
        sim = sim
      )
    },
    c("modelParameterSheets", "individualId", "applicationProtocol"),
    c(
      projectConfiguration$modelParamsFile,
      projectConfiguration$individualsFile,
      projectConfiguration$applicationsFile
    ),
    SIMPLIFY = FALSE
  )

  return(params)
}




#' Generate Population Files
#'
#' This function generates population files based on individual biometrics and parameters.
#'
#' @param dtTwinPops A data.table containing virtual twin population data.
#' @param params A list of parameters for the virtual twin population.
#' @param dtIndividualBiometrics A data.table containing individual biometrics.
#' @param projectConfiguration A list containing project configuration details.
#' @param sim A simulation object.
#'
#' @keywords internal
.generatePopulationFiles <- function(dtTwinPops, params, dtIndividualBiometrics, projectConfiguration, sim) {
  # initilaize variable to avoid warnings for global variables during check
  individualId <- NULL

  for (indId in dtIndividualBiometrics$individualId) {
    biomForInd <- dtIndividualBiometrics[individualId == indId, ]
    individualCharacteristics <- .createIndividualCharacteristics(biomForInd)
    individual <- ospsuite::createIndividual(individualCharacteristics)

    results <- .processIndividual(
      individual = individual,
      biomForInd = biomForInd,
      params = params,
      projectConfiguration = projectConfiguration,
      sim = sim
    )

    if (is.null(params$individualId[[indId]])) {
      params$individualId[[indId]] <- results
    } else {
      params$individualId[[indId]] <- utils::modifyList(params$individualId[[indId]], results)
    }
  }

  for (dPop in split(dtTwinPops, by = "populationName")) {
    poptable <- .buildVirtualTwinPopulation(
      projectConfiguration = projectConfiguration,
      params = params,
      dPop = unique(dPop)
    )

    .savePopulationFile(poptable, dPop, projectConfiguration)
  }
}

#' Create Individual Characteristics
#'
#' This function creates individual characteristics from biometrics data.
#'
#' @param biomForInd A data.table containing biometrics for an individual.
#'
#' @return An individual characteristics object.
#' @keywords internal
.createIndividualCharacteristics <- function(biomForInd) {
  moleculeOntogenies <- esqlabsR:::.readOntongeniesFromXLS(biomForInd)

  ospsuite::createIndividualCharacteristics(
    species = biomForInd$species,
    population = biomForInd$population,
    gender = biomForInd$gender,
    weight = biomForInd$`weight [kg]`,
    height = biomForInd$`height [cm]`,
    age = biomForInd$`age [year(s)]`,
    moleculeOntogenies = moleculeOntogenies
  )
}

#' Process Individual
#'
#' This function processes individual data and generates as results a list of all parameters transferred to population csv
#'
#' @param individual An individual object.
#' @param biomForInd A data.table containing biometrics for an individual.
#' @param params A list of parameters for the virtual twin population.
#' @param projectConfiguration A list containing project configuration details.
#' @param sim A simulation object.
#'
#' @return A list of results for the individual.
#' @keywords internal
.processIndividual <- function(individual, biomForInd, params, projectConfiguration, sim) {
  # avoid warnings during check
  paths <- NULL

  popParameters <- rbind(
    .convertBiomForIndStatics(biomForInd, sim),
    data.table::as.data.table(individual$derivedParameters)[paths %in% c("Organism|Weight", "Organism|BMI", "Organism|BSA")],
    data.table::as.data.table(individual$distributedParameters)
  )

  results <- stats::setNames(as.list(popParameters$values), popParameters$paths)

  return(results)
}



#' Save Population File
#'
#' This function saves the population data to a CSV file.
#'
#' @param poptable A data.table representing the population data.
#' @param dPop A data.table containing population metadata.
#' @param projectConfiguration A list containing project configuration details.
#'
#' @keywords internal
.savePopulationFile <- function(poptable, dPop, projectConfiguration) {
  # avoid warning for global variable
  individualId <- NULL

  poptable[, IndividualId := .I - 1]
  data.table::setcolorder(poptable, "IndividualId")

  utils::write.csv(
    x = poptable,
    file = file.path(
      projectConfiguration$populationsFolder,
      paste0(dPop$populationName[1], ".csv")
    ),
    fileEncoding = "UTF8",
    row.names = FALSE
  )

  return(invisible())
}


#' Build Virtual Twin Population
#'
#' This function builds an virtual twin population table based on the provided parameters and population data.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param params A list of parameters for the virtual twin population.
#' @param dPop A data.table containing population data.
#'
#' @return A data.table representing the virtual twin population.
#' @keywords internal
.buildVirtualTwinPopulation <- function(projectConfiguration, params, dPop) {
  poptable <- data.table()

  for (d in split(dPop, by = c("individualId", "dataGroups"))) {
    popRow <- list(populationName = d$populationName, dataGroup = d$dataGroup)

    for (parType in names(params)) {
      sheets <- .cleanUpSheetList(d[[parType]])

      for (sheet in sheets) {
        newValues <- params[[parType]][[sheet]]
        popRow <- utils::modifyList(popRow, newValues)
      }
    }

    if (nrow(poptable) > 1) {
      checkmate::assertNames(names(popRow), permutation.of = names(poptable))
    }

    poptable <- rbind(poptable, data.table::as.data.table(popRow))
  }

  return(poptable)
}

#' Get All Parameters for Sheets
#'
#' This function retrieves parameters from specified sheets in an Excel file.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param sheets A character vector of sheet names to read parameters from.
#' @param paramsXLSpath A string representing the path to the parameters Excel file.
#' @param sim A simulation object.
#'
#' @return A list of parameters for the specified sheets.
#' @keywords internal
.getAllParameterForSheets <- function(projectConfiguration, sheets, paramsXLSpath, sim) {
  # avoid warnings for global variables during check
  paths <- dimension <- values <- NULL

  wb <- openxlsx::loadWorkbook(paramsXLSpath)
  sheets <- intersect(openxlsx::sheets(wb), sheets)

  if (length(sheets) == 0) {
    return(list())
  }

  params <- lapply(sheets, function(sheet) {
    tmp <- esqlabsR::readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet) %>%
      data.table::as.data.table()
    if (nrow(tmp) == 0) stop(paste('empty table',paramsXLSpath,sheet))

    tmp[, `:=`(
      dimension = ospsuite::getParameter(paths, container = sim)$dimension
    ),
    by = "paths"
    ]
    tmp[, `:=`(
      baseValue = ospsuite::toBaseUnit(quantityOrDimension = dimension, values = values, unit = units)
    ),
    by = "paths"
    ]

    results <- stats::setNames(as.list(tmp$baseValue), tmp$paths)
    return(results)
  })

  names(params) <- sheets
  return(params)
}

#' Clean Up Sheet List
#'
#' This function sanitizes a list of sheet names by removing duplicates and whitespace.
#'
#' @param sheets A character vector of sheet names.
#'
#' @return A cleaned character vector of sheet names.
#' @keywords internal
.cleanUpSheetList <- function(sheets) {
  sheets <- unique(sheets)
  sheets <- unlist(strsplit(sheets, ","))
  sheets <- trimws(sheets)
  sheets <- sheets[!is.na(sheets) & sheets != ""]
  sheets <- unique(sheets)

  return(sheets)
}

#' Convert Biometrics for Individual Statistics
#'
#' This function converts biometrics data for individual statistics into a specific format.
#'
#' @param biomForInd A data.table containing biometrics for an individual.
#' @param sim A simulation object.
#'
#' @return A data.table with converted biometrics.
#' #' @keywords internal
.convertBiomForIndStatics <- function(biomForInd, sim) {
  # avoid messages for global variables during check
  paths <- NULL

  dtSelection <- data.table(
    pName = c("individualId", "Population", "Gender", "Height [cm]", "Age [year(s)]", "Gestational Age [week(s)]"),
    units = c("", "", "", "dm", "year(s)", "week(s)"),
    paths = c("ObservedIndividualId", "Population", "Gender", "Organism|Height", "Organism|Age", "Organism|Gestational age")
  )

  tmp <- biomForInd %>%
    dplyr::select(dplyr::any_of(dtSelection$pName)) %>%
    data.table::setnames(old = dtSelection$pName, new = dtSelection$paths, skip_absent = TRUE)

  if (!is.null(tmp$`Organism|Height`)) {
    tmp$`Organism|Height` <- ospsuite::toBaseUnit(
      quantityOrDimension = ospsuite::getParameter(path = "Organism|Height", container = sim)$dimension,
      unit = "cm",
      values = tmp$`Organism|Height`
    )
  }

  result <- data.table(paths = names(tmp), values = as.vector(unlist(tmp)))
  result[, units := dtSelection$units[match(paths, dtSelection$paths)]]

  return(result)
}
