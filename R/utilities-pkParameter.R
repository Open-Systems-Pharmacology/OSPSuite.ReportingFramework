# calculate ----------
#' Calculate and load PK Parameters
#'
#' This function calculates pharmacokinetic (PK) parameters for given scenarios based on project configuration and results.
#'
#' @param projectConfiguration A list containing project configuration, including output folder and PK parameter file.
#' @param scenarioResults A list of scenario results containing simulation data.
#' @param pkParameterSheets A vector with name of the sheets in the PK parameter file to read.
#'
#' @return A data.table containing combined PK analyses for all scenarios.
#'
#' @export
calculatePKParameter <- function(projectConfiguration,
                                 scenarioResults = NULL,
                                 pkParameterSheets) {
  checkmate::assertList(scenarioResults, types = "list", any.missing = FALSE, names = "named")
  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")
  checkmate::assertCharacter(pkParameterSheets, any.missing = FALSE)


  outputFolder <- file.path(projectConfiguration$outputFolder, "PKAnalysisResults")
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  if (nrow(dtOutputPaths) == 0) stop('Please define ouputPaths in plot configuration xlsx')

  initializeParametersOfSheets(projectConfiguration,pkParameterSheets)

  # Load or calculate PK analyses for all scenarios
  invisible(lapply(names(scenarioList), function(scenarioName) {

    message(paste("Calculate  PK analysis result of", scenarioName))

    pkAnalyses <- ospsuite::calculatePKAnalyses(results = scenarioResults[[scenarioName]]$results)

    ospsuite::exportPKAnalysesToCSV(
      pkAnalyses = pkAnalyses,
      filePath = file.path(outputFolder, paste0(scenarioName, '.csv'))
    )

  }))


  return(invisible())
}


#' Initialize Parameters of defined in given Sheets
#'
#' This function initialize pharmacokinetic (PK) parameters in specified sheets based on user-defined parameters.
#'
#' @param projectConfiguration A list containing project configuration, including the PK parameter file.
#' @param pkParameterSheets A vector of sheet names to update.
#' @return NULL (updates are made in-place).
initializeParametersOfSheets <- function(projectConfiguration, pkParameterSheets) {

  ospsuite::removeAllUserDefinedPKParameters()

  # Read user-defined PK parameters and clean the names
  dtUserdefPKParameter <- xlsxReadData(wb = projectConfiguration$addOns$pKParameterFile,
                                       sheetName = 'Userdef PK Parameter',
                                       skipDescriptionRow = TRUE) %>%
    stats::setNames(gsub(" \\[.*\\]", "", names(.)))

  checkmate::assertCharacter(dtUserdefPKParameter$name,any.missing = FALSE,unique = TRUE)
  if (any(is.na(dtUserdefPKParameter[['display Unit']]))) {
    stop("empty string is not possible as displayUnit in the sheet 'Userdef PK Parameter',
    workaround: use % and set displayUnit in sheet derived from template-sheet to empty string")
  }

  # Loop through each PK parameter sheet
  for (pkParameterSheet in pkParameterSheets) {

    dtPkParameterDefinition <- xlsxReadData(wb = projectConfiguration$addOns$pKParameterFile,
                                            sheetName = pkParameterSheet,
                                            skipDescriptionRow = TRUE)

    userdefinedParameters <- setdiff(dtPkParameterDefinition$name, ospsuite::allPKParameterNames())

    for (userPar in userdefinedParameters) {
      iRow <- which(dtUserdefPKParameter$name == userPar)

      if (length(iRow) == 0) {
        stop(paste('pkParameter', userPar, 'is not defined in "Userdef PK Parameter" sheet.'))
      }
      if (length(iRow) > 1) {
        stop(paste('pkParameter', userPar, 'is not unique in "Userdef PK Parameter" sheet.'))
      }

      myPK <- ospsuite::addUserDefinedPKParameter(
        name = dtUserdefPKParameter$name[iRow],
        standardPKParameter = StandardPKParameter[[dtUserdefPKParameter$`standard PK parameter`[iRow]]],
        displayUnit = dtUserdefPKParameter$`display Unit`[iRow]
      )

      for (col in setdiff(intersect(names(myPK), names(dtUserdefPKParameter)), c('name','dimension'))) {
        if (!is.na(dtUserdefPKParameter[[col]][iRow])) {
          myPK[[col]] <- dtUserdefPKParameter[[col]][iRow]
        }
      }
    }
  }

  # Update PK parameters in the specified sheets
  for (iRow in seq_len(nrow(dtPkParameterDefinition))) {
    ospsuite::updatePKParameter(
      name = dtPkParameterDefinition$name[iRow],
      displayName = dtPkParameterDefinition$displayName[iRow],
      displayUnit = dtPkParameterDefinition$displayUnit[iRow]
    )
  }

  return(invisible())
}

# load ----------
#' Load PK Parameter
#'
#' This function loads PK parameters for specified scenarios based on project configuration and scenario list.
#'
#' @param projectConfiguration A list containing project configuration, including the PK parameter file.
#' @param scenarioList A list of scenarios for which PK parameters are to be loaded.
#' @param pkParameterSheets A vector containing the names of the sheets in the PK parameter file to read.
#' @return A data.table containing the processed PK analyses.
#'
#' @export
loadPKParameter <- function(projectConfiguration,
                            scenarioList,
                            pkParameterSheets) {
  checkmate::assertList(scenarioList, types = "Scenario", any.missing = FALSE, names = "named",null.ok = TRUE)
  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")
  checkmate::assertCharacter(pkParameterSheets, any.missing = FALSE)

  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  if (nrow(dtOutputPaths) == 0) stop('Please define ouputPaths in plot configuration xlsx')

  #  initializeParametersOfSheets(projectConfiguration,pkParameterSheets)

  # Load or calculate PK analyses for all scenarios
  pkAnalysesList <- lapply(names(scenarioList), function(scenarioName) {
    loadPKAnalysisPerScenario(scenarioName = scenarioName,
                              scenario = scenarioList[[scenarioName]],
                              pkParameterSheet = pkParameterSheets,
                              projectConfiguration = projectConfiguration)
  })

  pkParameterDT <- merge(data.table::rbindlist(pkAnalysesList),
                         dtOutputPaths[, c('outputPathId', 'displayName')],
                         by = 'outputPathId',
                         suffixes = c('', 'Output'))

  return(pkParameterDT)
}


#' Load or Calculate PK Analysis for a Scenario
#'
#' This function loads PK analysis results from a CSV file .
#' It also processes each specified PK parameter sheet for the scenario.
#'
#' @param scenarioName The name of the scenario to process.
#' @param scenarioResult A list containing the results of the scenario.
#' @param pkParameterSheets A vector with names of the sheets in the PK parameter file to read.
#' @param projectConfiguration A list containing project configuration, including the PK parameter file.
#' @return A data.table containing the processed PK analyses.
#'
#' @keywords internal
loadPKAnalysisPerScenario <- function(scenarioName,scenario,
                                      pkParameterSheets, projectConfiguration) {

  dtPkAnalyses<- loadPkAnalysisRawData(projectConfiguration = projectConfiguration,
                                       scenarioName = scenarioName,
                                       scenario = scenario) %>%
    dplyr::mutate(scenarioName = scenarioName)

  dtOutputPaths <- getOutputPathIds(projectConfiguration)

  # Process each PK parameter sheet
  resultsList <- list()
  for (pkParameterSheet in pkParameterSheets) {

    dtPkParameterDefinition <- xlsxReadData(wb = projectConfiguration$addOns$pKParameterFile,
                                            sheetName = pkParameterSheet,
                                            skipDescriptionRow = TRUE)

    dtPkParameterDefinition <- addUnitFactorsToPKDefinition(scenario = scenario,
                                                            dtOutputPaths = dtOutputPaths,
                                                            dtPkAnalyses = dtPkAnalyses,
                                                            dtPkParameterDefinition = dtPkParameterDefinition)

    resultsList <- append(resultsList,
                          list(
                            dtPkAnalyses %>%
                              merge(
                                dtPkParameterDefinition,
                                by.x = c('parameter', 'quantityPath'),
                                by.y = c('name', 'outputPath')
                              ) %>%
                              dplyr::mutate(value = value * unitFactor) %>%
                              dplyr::select(
                                'scenarioName',
                                'parameter',
                                'individualId',
                                'value',
                                'outputPathId',
                                'displayName',
                                'displayUnit'
                              )
                          ))

  }

  return(data.table::rbindlist(resultsList))

}


#' Load PK Analysis from CSV
#'
#' This function loads PK analysis results from a CSV file or recalculates them if specified.
#'
#' @param projectConfiguration A list containing project configuration, including output folder and PK parameter file.
#' @param scenarioName name of scenario
#'
#' @return A data.table containing the PK analyses.
#'
#' @export
loadPkAnalysisRawData <- function(projectConfiguration, scenarioName,scenario) {

  outputFolder <- file.path(projectConfiguration$outputFolder, "PKAnalysisResults")
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  fileName  = file.path(outputFolder, paste0(scenarioName, '.csv'))
  if (!file.exists(fileName)) stop(paste('PK Parameter for',scenarioName,'calculated!'))

  message(paste("Load PK analysis result of", scenarioName))

  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = fileName,
    simulation = scenario$simulation
  )

  dtPkAnalyses <- ospsuite::pkAnalysesToDataFrame(pkAnalyses = pkAnalyses) %>%
    data.table::setDT() %>%
    setHeadersToLowerCase()

  dtPkAnalyses[is.na(unit),unit:='']


  return(dtPkAnalyses)
}

#' Process PK Parameter Definitions
#'
#' This function processes the PK parameter definitions for a specific scenario, including unit conversions and output filters.
#'
#' @param scenarioResult A list containing the results of the scenario.
#' @param dtOutputPaths A data.table containing output paths.
#' @param dtPkAnalyses A data.table containing PK analyses.
#' @param dtPkParameterDefinition A data.table containing PK parameter definitions.
#' @return A data.table containing processed PK parameter definitions for the scenario.
#' @export
addUnitFactorsToPKDefinition <- function(scenario,
                                         dtOutputPaths,
                                         dtPkAnalyses,
                                         dtPkParameterDefinition) {
  # Initialize variables to avoid linter messages
  outputPathIds <- unitFactor <- dimension <- NULL

  # Select relevant columns from dtOutputPaths and merge with dtPkAnalyses
  # This creates a dataset (dtO) that contains output path IDs and their associated parameters
  dtO <- dtOutputPaths %>%
    dplyr::select(c("outputPathId", "outputPath")) %>%
    merge(
      dtPkAnalyses %>%
        dplyr::select("parameter", "quantityPath", "unit") %>%
        unique(),
      by.x = "outputPath",
      by.y = "quantityPath"
    )

  # Populate molweight for each unique output path based on the scenario's molWeightFor function
  dtO[, molweight := NA_real_]

  for (pt in unique(dtO$outputPath)) {
    dtO[outputPath == pt, molweight := scenario$simulation$molWeightFor(pt)]
  }

  # Remove descriptions from dtPkParameterDefinition and handle NA outputPathIds
  dtPkParameterDefinition <- dtPkParameterDefinition %>%
    dplyr::select(!any_of(c("descriptions")))

  # If outputPathIds are NA, concatenate unique outputPathIds from dtO
  dtPkParameterDefinition[is.na(outputPathIds), outputPathIds := paste(unique(dtO$outputPathId), collapse = ", ")]

  # Split outputPathIds into a list and merge with dtO to create a comprehensive parameter definition
  dtPkParameterDefinition <-
    dtPkParameterDefinition[, .(outputPathId = splitInputs(outputPathIds)),
                            by = c("name", "displayName", "displayUnit")
    ] %>%
    merge(dtO,
          by.x = c("name", "outputPathId"),
          by.y = c("parameter", "outputPathId")
    )

  # Check if the resulting dtPkParameterDefinition is empty; if so, stop execution with an error message
  if (nrow(dtPkParameterDefinition) == 0) stop('PK Parameter definitions does not match with scenario outputs')

  # Calculate unit factors for each row in dtPkParameterDefinition
  for (iRow in seq_len(nrow(dtPkParameterDefinition))) {
    dtPkParameterDefinition$unitFactor[iRow] <-
      ospsuite::toUnit(
        quantityOrDimension = ospsuite::getDimensionForUnit(dtPkParameterDefinition$unit[iRow]),
        values = 1,
        sourceUnit = dtPkParameterDefinition$unit[iRow],
        targetUnit = dtPkParameterDefinition$displayUnit[iRow],
        molWeight = as.double(dtPkParameterDefinition$molweight[iRow])
      )
  }

  # Return the modified dtPkParameterDefinition with unit factors included
  return(dtPkParameterDefinition)
}





