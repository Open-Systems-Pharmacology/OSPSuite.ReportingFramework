#' Calculate and load PK Parameters
#'
#' This function calculates pharmacokinetic (PK) parameters for given scenarios based on project configuration and results.
#'
#' @param projectConfiguration A list containing project configuration, including output folder and PK parameter file.
#' @param scenarioResults A list of scenario results containing simulation data.
#' @param pkParameterSheets A vector with name of the sheets in the PK parameter file to read.
#' @param withRecalculation A boolean indicating whether to recalculate PK parameters if results already exist.
#'
#' @return A data.table containing combined PK analyses for all scenarios.
#'
#' @export
calculateAndLoadPKParameter <- function(projectConfiguration,
                                        scenarioResults,
                                        pkParameterSheets,
                                        withRecalculation = TRUE) {
  checkmate::assertList(scenarioResults, types = "list", any.missing = FALSE, names = "named")
  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")
  checkmate::assertCharacter(pkParameterSheets, any.missing = FALSE)
  checkmate::assertFlag(withRecalculation)

  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  if (nrow(dtOutputPaths) == 0) stop('Please define ouputPaths in plot configuration xlsx')

  updateParameterOfSheets(projectConfiguration,pkParameterSheets)

  # Load or calculate PK analyses for all scenarios
  pkAnalysesList <- lapply(names(scenarioResults), function(scenarioName) {
    loadPKAnalysisPerScenario(scenarioName = scenarioName,
                              scenarioResult = scenarioResults[[scenarioName]],
                              pkParameterSheet = pkParameterSheets,
                              withRecalculation = withRecalculation,
                              projectConfiguration = projectConfiguration)
  })

  return(data.table::rbindlist(pkAnalysesList))
}


#' Load or Calculate PK Analysis for a Scenario
#'
#' This function loads PK analysis results from a CSV file or recalculates them if specified.
#' It also processes each specified PK parameter sheet for the scenario.
#'
#' @param scenarioName The name of the scenario to process.
#' @param outputFolder The folder where output files are stored.
#' @param scenarioResult A list containing the results of the scenario.
#' @param pkParameterSheets A vector with names of the sheets in the PK parameter file to read.
#' @param withRecalculation A boolean indicating whether to recalculate PK parameters if results already exist.
#' @param projectConfiguration A list containing project configuration, including the PK parameter file.
#' @return A data.table containing the processed PK analyses.
loadPKAnalysisPerScenario <- function(scenarioName,scenarioResult,
                                      pkParameterSheets, withRecalculation, projectConfiguration) {

  dtPkAnalyses<- loadPkAnalysis(projectConfiguration = projectConfiguration,
                                scenarioName = scenarioName,
                              scenarioResult = scenarioResult,
                              withRecalculation = withRecalculation) %>%
    dplyr::mutate(scenarioName = scenarioName)

  dtOutputPaths <- getOutputPathIds(projectConfiguration)

  # Process each PK parameter sheet
  resultsList <- list()
  for (pkParameterSheet in pkParameterSheets) {

    dtPkParameterDefinition <- xlsxReadData(wb = projectConfiguration$addOns$pKParameterFile,
                                            sheetName = pkParameterSheet,
                                            skipDescriptionRow = TRUE)

    dtPkParameterDefinition <- processPKDefinition(scenarioResult,
                                                   dtOutputPaths,
                                                   dtPkAnalyses,
                                                   dtPkParameterDefinition)

    resultsList <- append(resultsList,
                          list(processPKAnalyses(dtPkAnalyses, dtPkParameterDefinition)))
  }

  return(data.table::rbindlist(resultsList))

}


#' Process PK Analyses with Parameter Definitions
#'
#' This function processes the PK analyses using the provided parameter definitions.
#'
#' @param pkAnalyses A data.table containing PK analyses.
#' @param dtPkParameterDefinition A data.table containing PK parameter definitions.
#' @param dtOutputPaths A data.table containing output paths.
#' @return A processed data.table containing PK analyses with parameters.
#' @keywords internal
processPKAnalyses <- function(pkAnalyses, dtPkParameterDefinition, dtOutputPaths) {
  dtPkAnalyses <- pkAnalyses %>%
    merge(dtPkParameterDefinition, by.x = c('parameter', 'quantityPath'), by.y = c('name', 'paths'))

  dtPkAnalyses[, value := value * unitFactor]
  dtPkAnalyses <- dtPkAnalyses %>%
    dplyr::select('scenarioName','parameter', 'individualId', 'value', 'outputPathId', 'displayName', 'displayUnit')

  return(dtPkAnalyses)
}


#' Load PK Analysis from CSV
#'
#' This function loads PK analysis results from a CSV file or recalculates them if specified.
#'
#' @param fileName The path to the CSV file containing PK analysis results.
#' @param scenarioResult A list containing the results of the scenario.
#' @param withRecalculation A boolean indicating whether to recalculate PK parameters if results already exist.
#' @return A data.table containing the PK analyses.
#'
#' @export
loadPkAnalysis <- function(projectConfiguration, scenarioName, scenarioResult, withRecalculation) {

  outputFolder <- file.path(projectConfiguration$outputFolder, "PKAnalysisResults")
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  fileName  = file.path(outputFolder, paste0(scenarioName, '.csv'))
  if (file.exists(fileName) & !withRecalculation) {
    message(paste("Load PK analysis result of", fileName))

    pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
      filePath = fileName,
      simulation = scenarioResult$simulation
    )
  } else {
    message(paste("Calculate  PK analysis result of", fileName))

    pkAnalyses <- ospsuite::calculatePKAnalyses(results = scenarioResult$results)

    ospsuite::exportPKAnalysesToCSV(
      pkAnalyses = pkAnalyses,
      filePath = fileName
    )

  }

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
processPKDefinition <- function(scenarioResult,
                                dtOutputPaths,
                                dtPkAnalyses,
                                dtPkParameterDefinition) {
  # initial variables to avoid linter message
  outputPathIds <- unitFactor <- dimension <- NULL

  # select all information for unit conversion and output filter
  dtO <- simulationResultsToDataFrame(scenarioResult$results) %>%
    dplyr::select(c("paths", "molWeight")) %>%
    unique() %>%
    merge(
      dtOutputPaths %>%
        dplyr::select(c("outputPathId", "outputPath")),
      by.x = "paths",
      by.y = "outputPath"
    ) %>%
    merge(
      dtPkAnalyses %>%
        dplyr::select("parameter", "quantityPath", "unit") %>%
        unique(),
      by.x = "paths",
      by.y = "quantityPath"
    )

  dtPkParameterDefinition <- dtPkParameterDefinition %>%
    dplyr::select(!c("descriptions"))
  dtPkParameterDefinition[is.na(outputPathIds), outputPathIds := paste(unique(dtO$outputPathId), collapse = ", ")]
  dtPkParameterDefinition <-
    dtPkParameterDefinition[, .(outputPathId = splitInputs(outputPathIds)),
      by = c("name", "displayName", "displayUnit")
    ] %>%
    merge(dtO,
      by.x = c("name", "outputPathId"),
      by.y = c("parameter", "outputPathId")
    )

  dtPkParameterDefinition[, unitFactor := apply(dtPkParameterDefinition, 1, function(row) {
    ospsuite::toUnit(
      # ospsuite::pkParameterByName(row["name"])$dimension, That does not work as userdefined PK Parameter do not change dimension (e.g.F_tEnd uses C_trogh and is a Fraction)
      quantityOrDimension = ospsuite::getDimensionForUnit(row[["unit"]]),
      values = 1,
      sourceUnit = row["unit"],
      targetUnit = row["displayUnit"],
      molWeight = as.double(row["molWeight"]),
      molWeightUnit = "g/mol"
    )
  })]
  return(dtPkParameterDefinition)
}


#' Update Parameters of Sheets
#'
#' This function updates pharmacokinetic (PK) parameters in specified sheets based on user-defined parameters.
#'
#' @param projectConfiguration A list containing project configuration, including the PK parameter file.
#' @param pkParameterSheets A vector of sheet names to update.
#' @return NULL (updates are made in-place).
updateParameterOfSheets <- function(projectConfiguration, pkParameterSheets) {

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

  return(NULL)
}


