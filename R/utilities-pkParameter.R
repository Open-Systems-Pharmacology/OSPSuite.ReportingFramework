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
                                 scenarioResult,
                                 pkParameterSheets,
                                 scenarioName) {
  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")
  checkmate::assertList(scenarioResult, any.missing = FALSE, names = "named")
  checkmate::assertString(scenarioName)

  initializeParametersOfSheets(projectConfiguration, pkParameterSheets)

  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults)
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
  if (nrow(dtOutputPaths) == 0) stop("Please define ouputPaths in plot configuration xlsx")


  # Load or calculate PK analyses

  writeToLog(type = 'Info',
             msg = paste("Calculate  PK analysis result of", scenarioName))

  pkAnalyses <- ospsuite::calculatePKAnalyses(results = scenarioResult$results)

  ospsuite::exportPKAnalysesToCSV(
    pkAnalyses = pkAnalyses,
    filePath = file.path(outputFolder, paste0(scenarioName, ".csv"))
  )

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
  dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  ospsuite::removeAllUserDefinedPKParameters()

  # Read user-defined PK parameters and clean the names
  dtUserdefPKParameter <- xlsxReadData(
    wb = projectConfiguration$addOns$pKParameterFile,
    sheetName = "Userdef PK Parameter",
    skipDescriptionRow = TRUE
  ) %>%
    stats::setNames(gsub(" \\[.*\\]", "", names(.)))

  checkmate::assertCharacter(dtUserdefPKParameter$name, any.missing = FALSE, unique = TRUE)
  if (any(is.na(dtUserdefPKParameter[["display Unit"]]))) {
    stop("empty string is not possible as displayUnit in the sheet 'Userdef PK Parameter',
    workaround: use % and set displayUnit in sheet derived from template-sheet to empty string")
  }

  # Loop through each PK parameter sheet
  for (pkParameterSheet in splitInputs(pkParameterSheets)) {
    dtPkParameterDefinition <- xlsxReadData(
      wb = projectConfiguration$addOns$pKParameterFile,
      sheetName = pkParameterSheet,
      skipDescriptionRow = TRUE
    )

    userdefinedParameters <- setdiff(dtPkParameterDefinition$name, ospsuite::allPKParameterNames())

    for (userPar in userdefinedParameters) {
      iRow <- which(dtUserdefPKParameter$name == userPar)

      if (length(iRow) == 0) {
        stop(paste("pkParameter", userPar, 'is not defined in "Userdef PK Parameter" sheet.'))
      }
      if (length(iRow) > 1) {
        stop(paste("pkParameter", userPar, 'is not unique in "Userdef PK Parameter" sheet.'))
      }

      myPK <- ospsuite::addUserDefinedPKParameter(
        name = dtUserdefPKParameter$name[iRow],
        standardPKParameter = StandardPKParameter[[dtUserdefPKParameter$`standard PK parameter`[iRow]]],
        displayUnit = dtUserdefPKParameter$`display Unit`[iRow]
      )

      for (col in setdiff(intersect(names(myPK), names(dtUserdefPKParameter)), c("name", "dimension"))) {
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
#' @return A data.table containing the processed PK analyses.
#'
#' @export
loadPKParameter <- function(projectConfiguration,
                            scenarioList) {
  checkmate::assertList(scenarioList, types = "Scenario", any.missing = FALSE, names = "named", null.ok = TRUE)
  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")

  dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
  if (nrow(dtOutputPaths) == 0) stop("Please define ouputPaths in plot configuration xlsx")

  # Load or calculate PK analyses for all scenarios
  pkAnalysesList <- lapply(names(scenarioList), function(sc) {
    pkParameterSheets <- dtScenarios[scenarioName == sc & !is.na(pKParameter)]$pKParameter

    loadPKAnalysisPerScenario(
      scenarioName = sc,
      scenario = scenarioList[[sc]],
      pkParameterSheet = pkParameterSheets,
      projectConfiguration = projectConfiguration
    )
  })
  pkParameterDT <- merge(data.table::rbindlist(pkAnalysesList),
                         dtScenarios[, c("scenarioName", "populationId")],
                         by.x = "scenario",
                         by.y = "scenarioName"
  )

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
loadPKAnalysisPerScenario <- function(scenarioName, scenario,
                                      pkParameterSheets, projectConfiguration) {
  dtPkAnalyses <- loadPkAnalysisRawData(
    projectConfiguration = projectConfiguration,
    scenarioName = scenarioName,
    scenario = scenario
  ) %>%
    dplyr::mutate(scenario = scenarioName)

  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)

  # Process each PK parameter sheet
  resultsList <- list()
  for (pkParameterSheet in splitInputs(pkParameterSheets)) {
    dtPkParameterDefinition <- xlsxReadData(
      wb = projectConfiguration$addOns$pKParameterFile,
      sheetName = pkParameterSheet,
      skipDescriptionRow = TRUE
    )

    dtPkParameterDefinition <- addUnitFactorsToPKDefinition(
      scenario = scenario,
      dtOutputPaths = dtOutputPaths,
      dtPkAnalyses = dtPkAnalyses,
      dtPkParameterDefinition = dtPkParameterDefinition
    )

    resultsList <- append(
      resultsList,
      list(
        dtPkAnalyses %>%
          merge(
            dtPkParameterDefinition,
            by.x = c("parameter", "quantityPath"),
            by.y = c("name", "outputPath")
          ) %>%
          dplyr::mutate(value = value * unitFactor) %>%
          dplyr::select(
            "scenario",
            "parameter",
            "individualId",
            "value",
            "outputPathId",
            "displayName",
            "displayUnit"
          ) %>%
          setnames(
            old = c("parameter","displayName", "displayUnit"),
            new = c("pkParameter","displayNamePKParameter", "displayUnitPKParameter")
          )
      )
    )
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
loadPkAnalysisRawData <- function(projectConfiguration, scenarioName, scenario) {
  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults)
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  fileName <- file.path(outputFolder, paste0(scenarioName, ".csv"))
  if (!file.exists(fileName)) stop(paste("PK Parameter for", scenarioName, "calculated!"))

  writeToLog(type = 'Info',
             msg = paste("Load PK analysis result of", scenarioName))

  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = fileName,
    simulation = scenario$simulation
  )

  dtPkAnalyses <- ospsuite::pkAnalysesToDataFrame(pkAnalyses = pkAnalyses) %>%
    data.table::setDT() %>%
    setHeadersToLowerCase()

  dtPkAnalyses[is.na(unit), unit := ""]


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
  if (nrow(dtPkParameterDefinition) == 0) stop("PK Parameter definitions does not match with scenario outputs")

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
#plot Support --------
mergePKParameterWithConfigTable <- function(onePlotConfig,
                                            pkParameterDT,
                                            colorVector = NULL,
                                            asRatio = FALSE) {

  # initialize to avoid linter messages
  displayNameOutput <- plotTag <- isReference <- isReference <- colorIndex <- referenceScenario <- NULL

  onePlotConfig <- data.table::copy(onePlotConfig)
  for (col in intersect(names(onePlotConfig),
                        c("scenarios","pkParameters","outputPathIds"))){
    onePlotConfig <- separateAndTrim(onePlotConfig, col)
  }
  mergedData <- onePlotConfig %>%
    dplyr::select(dplyr::any_of(c("plotName","scenario","referenceScenario", "pkParameter", "outputPathId",
                                   "scenarioShortName","scenarioLongName"))) %>%
    merge(
      pkParameterDT %>%
        unique(),
      by = c("scenario", "pkParameter", "outputPathId")
    ) %>%
    merge(configEnv$outputPaths[, c("outputPathId", "displayNameOutput")],
          by = "outputPathId"
    )

  if (nrow(mergedData) == 0) stop(paste('no PK-Parameter available for',onePlotConfig$plotName[1]))


  if (asRatio) {
    mergedData <- setValueToRatio(mergedData, pkParameterDT)
  } else if (!is.null(colorVector)) {
    mergedData[, isReference := scenario %in% referenceScenario, by = c("plotName")]
    mergedData[, colorIndex := ifelse(isReference == TRUE, names(colorVector)[2], names(colorVector)[1])]
    mergedData$colorIndex <- factor(mergedData$colorIndex, levels = names(colorVector))
  }

  # Ensure order by creating factors
  mergedData$displayNameOutput <- factor(mergedData$displayNameOutput,
                                         levels = unique(mergedData$displayNameOutput),
                                         ordered = TRUE
  )

  mergedData$scenarioShortName <- factor(mergedData$
                                           scenarioShortName,
                                         levels = unique(onePlotConfig$scenarioShortName),
                                         ordered = TRUE
  )

  mergedData$scenarioLongName <- factor(mergedData$
                                           scenarioLongName,
                                         levels = unique(onePlotConfig$scenarioLongName),
                                         ordered = TRUE
  )

  return(mergedData)
}
#' Set Value to Ratio
#'
#' This function computes the ratio of values between base and reference scenarios
#' for specified pharmacokinetic parameters. It merges the configuration data with
#' pharmacokinetic parameter data and calculates the ratio based on the provided
#' individual IDs and output path IDs.
#'
#' @param mergedData A data.table containing configuration data with columns for
#'               referenceScenario, pkParameter, individualId, and outputPathId.
#' @param pkParameterDT A data.table containing pharmacokinetic parameter data
#'                      including scenario names, parameters, individual IDs,
#'                      output path IDs, values, and population IDs.
#'
#' @return A data.table containing the merged data with column `value`
#'         representing the ratio of base to reference values.
#'
#' @keywords internal
setValueToRatio <- function(mergedData, pkParameterDT) {

  mergedData <- merge(mergedData,
                      pkParameterDT[, c("scenario", "pkParameter", "individualId", "outputPathId", "value", "populationId")] %>%
                        setnames(
                          old = c("scenario"),
                          new = c("referenceScenario")
                        ),
                      by = c("referenceScenario", "pkParameter", "individualId", "outputPathId"),
                      suffixes = c(".base", ".reference")
  )
  mergedData[, value := value.base / value.reference]


  return(mergedData)
}

#' Validate Pharmacokinetic Parameter Data Table
#'
#' This function validates the structure and content of the provided
#' pharmacokinetic parameter data table (`pkParameterDT`). It checks
#' whether the data table has the required columns and ensures that the
#' combination of `outputPathId` and `parameter` is unique with consistent
#' `displayUnitPKParameter`.
#'
#' @param pkParameterDT A data.table containing pharmacokinetic parameters
#'                      with the following required columns:
#'                      scenario, pkParameter, individualId, value,
#'                      outputPathId, displayNamePKParameter,
#'                      and displayUnitPKParameter.
#'
#' @return None. The function will stop execution if validation fails,
#'         otherwise returns invisibly.
#'
#' @keywords internal
validatePKParameterDT <- function(pkParameterDT) {
  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenario", "pkParameter", "individualId", "value", "outputPathId", "displayNamePKParameter", "displayUnitPKParameter"))

  tmp <- pkParameterDT[, c("pkParameter", "outputPathId", "displayUnitPKParameter")] %>%
    unique()

  if (any(duplicated(tmp[, c("outputPathId", "pkParameter")]))) {
    stop("Please check pkParameterDT. It seems that displayUnitPKParameter is not consistent for outputPathId and pkParameter")
  }

  return(invisible())
}
