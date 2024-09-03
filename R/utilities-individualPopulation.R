#' Setup Individual Population Configuration
#'
#' This function sets up a configuration sheet for individual populations based on the provided project configuration and observed data.
#'
#' @param projectConfiguration A list containing project configuration details, including file paths for populations and scenarios.
#' @param dataObserved A data object containing observed data, which may be of class "DataCombined".
#' @param groups A character vector of group names to filter the individual population. Defaults to NULL.
#'
#' @return Returns NULL invisibly if no groups are available for individual population creation. Modifies the workbook specified in projectConfiguration.
#' @export
#'
#' @examples
#' # Example usage:
#' setupIndPopConfig(projectConfiguration, dataObserved, groups = c("Group1", "Group2"))
setupIndPopConfig <- function(projectConfiguration, dataObserved, groups = NULL) {

  checkmate::assertCharacter(groups, any.missing = FALSE, null.ok = TRUE)

  # Check if dataObserved is of class "DataCombined" and convert it if necessary
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  # Load configuration table for IndividualPopulation
  wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

  # Check if the 'IndividualPopulation' sheet exists
  if (!('IndividualPopulation' %in% wb$sheet_names)) {
    dtIndPops <- xlsxReadData(projectConfiguration$scenariosFile, sheetName = 'Scenarios') %>%
      data.table::setnames("PopulationId", 'PopulationName') %>%
      dplyr::mutate('DataGroup' = '') %>%
      dplyr::select(c('PopulationName', 'DataGroup', "IndividualId", "ModelParameterSheets", "ApplicationProtocol")) %>%
      dplyr::filter(FALSE)
  } else {
    dtIndPops <- xlsxReadData(wb, 'IndividualPopulation')
  }

  # Remove any groups that are already in dtIndPops
  groups <- setdiff(groups, unique(dtIndPops$DataGroup))
  groups <- getIndividualDataGroups(dataObserved, groups)

  # Check if any groups are available for individual population creation
  if (length(groups) == 0) {
    warning("No groups available for individual population creation")
    return(NULL)
  }

  # Create new individual population data
  dtIndPopsNew <- dataObserved[group %in% groups, c('group', 'individualId')] %>%
    unique() %>%
    dplyr::mutate(PopulationName = group) %>%
    data.table::setnames(old = c('group', 'individualId'),
                         new = c('DataGroup', 'IndividualId')) %>%
    data.table::setorderv(c('DataGroup', 'IndividualId'))

  # Combine the existing and new individual population data
  dtIndPops <- rbind(dtIndPops, dtIndPopsNew, fill = TRUE)

  # Write data to the workbook
  if (!('IndividualPopulation' %in% wb$sheet_names)) {
    xlsxAddSheet(wb = wb, sheetName = 'IndividualPopulation', dt = dtIndPops)
  } else {
    xlsxWriteData(wb = wb, sheetName = 'IndividualPopulation', dt = dtIndPops)
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$populationsFile, overwrite = TRUE)

  return(invisible())
}


#' Generate Individual Population
#'
#' This function generates individual populations based on the provided model and project configuration.
#'
#' @param projectConfiguration A list containing project configuration details, including file paths for populations and scenarios.
#' @param modelFile A string representing the name of the model file to be loaded.
#' @param overwrite A logical indicating whether to overwrite existing files. Defaults to FALSE
#'
#' @export
#'
#' @examples
#' # Example usage:
#' generateIndividualPopulation(projectConfiguration, "modelFileName", overwrite = TRUE)
generateIndividualPopulation <- function(projectConfiguration, modelFile, overwrite = FALSE) {

  dtIndPops <- xlsxReadData(wb = projectConfiguration$populationsFile,
                            sheetName = "IndividualPopulation",
                            emptyAsNA = FALSE)

  # Check if overwrite is FALSE and filter for existing files
  if (!overwrite) {
    existingFiles <- list.files(projectConfiguration$populationsFolder, pattern = "*.csv", full.names = TRUE)
    existingPopulationNames <- sub("\\.csv$", "", basename(existingFiles))

    # Filter dtIndPops for populations that do not exist
    dtIndPops <- dtIndPops[!PopulationName %in% existingPopulationNames]
  }
  # If no populations left to generate, return with a message
  if (nrow(dtIndPops) == 0) {
    message("No new individual populations to generate; all files already exist.")
    return(invisible())
  }

  sim <-   ospsuite::loadSimulation(file.path(projectConfiguration$modelFolder, modelFile))

  params <- .readParameterSheetList(projectConfiguration, dtIndPops, sim)

  dtIndividualBiometrics <- xlsxReadData(wb = projectConfiguration$individualsFile, sheetName = "IndividualBiometrics")
  dtIndividualBiometrics[IndividualId %in% dtIndPops$IndividualId]


  .generatePopulationFiles(dtIndPops, params, dtIndividualBiometrics, projectConfiguration,sim)

  .addMissingScenarios(projectConfiguration = projectConfiguration,dtIndPops = dtIndPops,modelFile = modelFile)

  return(invisible())
}


#' Read Parameter Sheet List
#'
#' This function reads parameters from specified sheets in an Excel file.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param dtIndPops A data.table containing individual population data.
#' @param sim A simulation object.
#'
#' @return A list of parameters for the specified sheets.
#' @keywords internal
.readParameterSheetList <- function(projectConfiguration, dtIndPops, sim) {
  params <- mapply(
    function(sheet, file) {
      .getAllParameterForSheets(
        projectConfiguration = projectConfiguration,
        sheets = .cleanUpSheetList(dtIndPops[[sheet]]),
        paramsXLSpath = file,
        sim = sim
      )
    },
    c('ModelParameterSheets', 'IndividualId', 'ApplicationProtocol'),
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
#' @param dtIndPops A data.table containing individual population data.
#' @param params A list of parameters for the individual population.
#' @param dtIndividualBiometrics A data.table containing individual biometrics.
#' @param projectConfiguration A list containing project configuration details.
#' @param sim A simulation object.
#'
#' @keywords internal
.generatePopulationFiles <- function(dtIndPops, params, dtIndividualBiometrics, projectConfiguration,sim) {

  for (indId in dtIndividualBiometrics$IndividualId) {

    biomForInd <- dtIndividualBiometrics[IndividualId == indId, ]
    individualCharacteristics <- .createIndividualCharacteristics(biomForInd)
    individual <- createIndividual(individualCharacteristics)

    results <- .processIndividual(individual = individual,
                                  biomForInd = biomForInd,
                                  params = params,
                                  projectConfiguration = projectConfiguration,
                                  sim = sim)

    if (is.null(params$IndividualId[[indId]])) {
      params$IndividualId[[indId]] <- results
    } else {
      params$IndividualId[[indId]] <- utils::modifyList(params$IndividualId[[indId]], results)
    }
  }

  for (dPop in split(dtIndPops, by = "PopulationName")) {
    poptable <- .buildIndividualPopulation(projectConfiguration = projectConfiguration,
                                           params = params,
                                           dPop = dPop)

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
    species = biomForInd$Species,
    population = biomForInd$Population,
    gender = biomForInd$Gender,
    weight = biomForInd$`Weight [kg]`,
    height = biomForInd$`Height [cm]`,
    age = biomForInd$`Age [year(s)]`,
    moleculeOntogenies = moleculeOntogenies
  )
}

#' Process Individual
#'
#' This function processes individual data and generates as results a list of all paremetrs transfreed to poulation csv
#'
#' @param individual An individual object.
#' @param biomForInd A data.table containing biometrics for an individual.
#' @param params A list of parameters for the individual population.
#' @param projectConfiguration A list containing project configuration details.
#' @param sim A simulation object.
#'
#' @return A list of results for the individual.
#' @keywords internal
.processIndividual <- function(individual, biomForInd, params, projectConfiguration,sim) {
  popParameters <- rbind(
    .convertBiomForIndStatics(biomForInd,sim),
    data.table::as.data.table(individual$derivedParameters)[paths %in% c("Organism|Weight", "Organism|BMI", "Organism|BSA")],
    data.table::as.data.table(individual$distributedParameters)
  )

  results <- setNames(as.list(popParameters$values), popParameters$paths)

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
  poptable[, IndividualId := .I - 1]
  data.table::setcolorder(poptable, 'IndividualId')

  write.csv(x = poptable,
            file = file.path(
              projectConfiguration$populationsFolder,
              paste0(dPop$PopulationName[1], '.csv')
            ),
            fileEncoding = 'UTF8',
            row.names = FALSE)

  return(invisible())
}


#' Build Individual Population
#'
#' This function builds an individual population table based on the provided parameters and population data.
#'
#' @param projectConfiguration A list containing project configuration details.
#' @param params A list of parameters for the individual population.
#' @param dPop A data.table containing population data.
#'
#' @return A data.table representing the individual population.
#' @keywords internal
.buildIndividualPopulation <- function(projectConfiguration, params, dPop) {

  poptable <- data.table()

  for (d in split(dPop, by = c('IndividualId', 'DataGroup'))) {
    popRow <- list(PopulationName = d$PopulationName, DataGroup = d$DataGroup)

    for (parType in names(params)) {
      sheets = .cleanUpSheetList(d[[parType]])

      for (sheet in sheets) {
        newValues = params[[parType]][[sheet]]
        popRow <- utils::modifyList(popRow, newValues)
      }
    }

    if (nrow(poptable) > 1){
      checkmate::assertNames(names(popRow),permutation.of = names(poptable))
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

  wb <- openxlsx::loadWorkbook(paramsXLSpath)
  sheets <- intersect(openxlsx::sheets(wb), sheets)

  if (length(sheets) == 0) {
    return(list())
  }

  params <- lapply(sheets, function(sheet) {
    tmp <- readParametersFromXLS(paramsXLSpath = paramsXLSpath, sheets = sheet) %>%
      data.table::as.data.table()

    tmp[, `:=`(
      dimension = getParameter(paths, container = sim)$dimension),
      by = 'paths']
    tmp[, `:=`(
      base_value = toBaseUnit(quantityOrDimension = dimension, values = values, unit = units)),
      by = 'paths']

    results = setNames(as.list(tmp$base_value), tmp$paths)
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
  sheets <- unlist(strsplit(sheets, ','))
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
.convertBiomForIndStatics <- function(biomForInd,sim) {

  dtSelection <- data.table(
    pName = c("IndividualId", "Population", "Gender", "Height [cm]", "Age [year(s)]", "Gestational Age [week(s)]"),
    units = c('', '', '', 'dm', 'year(s)', 'week(s)'),
    paths = c("ObservedIndividualId", "Population", "Gender", "Organism|Height", "Organism|Age", "Organism|Gestational age")
  )

  tmp <- biomForInd %>%
    dplyr::select(any_of(dtSelection$pName)) %>%
    data.table::setnames(old = dtSelection$pName, new = dtSelection$paths, skip_absent = TRUE)

  if (!is.null(tmp$`Organism|Height`)) {
    tmp$`Organism|Height` <- ospsuite::toBaseUnit(
      quantityOrDimension = getParameter(path = 'Organism|Height', container = sim)$dimension,
      unit = 'cm',
      values = tmp$`Organism|Height`
    )
  }

  result <- data.table(paths = names(tmp), values = as.vector(unlist(tmp)))
  result[, units := dtSelection$units[match(paths, dtSelection$paths)]]

  return(result)
}

#' Add Missing Scenarios
#'
#' This function adds any missing scenarios to the scenarios workbook based on the provided individual population data.
#'
#' @param dtIndPops A data.table containing individual population data with PopulationName.
#' @param projectConfiguration A list containing project configuration details, including the scenarios file path.
#' @param modelFile A string representing the name of the model file to be used.
#'
#' @return Returns NULL invisibly after updating the scenarios workbook.
#' @keywords internal
.addMissingScenarios <- function(dtIndPops, projectConfiguration, modelFile) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dtScenario <- xlsxReadData(wb, 'Scenarios')

  newScenarios <- setdiff(unique(dtIndPops$PopulationName), dtScenario$PopulationId)

  if (length(newScenarios) > 0) {
    dtScenario <- rbind(dtScenario,
                        data.table(Scenario_name = tolower(newScenarios),
                                   PopulationId = newScenarios,
                                   ReadPopulationFromCSV = TRUE,
                                   ModelFile = modelFile),
                        fill = TRUE)

    xlsxWriteData(wb = wb, sheetName = 'Scenarios', dt = dtScenario)
    openxlsx::saveWorkbook(wb, projectConfiguration$scenariosFile, overwrite = TRUE)
  }
  return(invisible())
}
