# calculate ----------
#' Calculate and Load Pharmacokinetic (PK) Parameters
#'
#' This function calculates pharmacokinetic (PK) parameters for specified scenarios
#' based on project configuration and simulation results. It generates output files
#' for each scenario in the designated output folder.
#'
#' @param projectConfiguration A list containing project configuration settings,
#' including the output folder path and the PK parameter file.
#' @param scenarioResults A named list of scenario results, each containing
#' simulation data for PK analysis.
#'
#' @return This function is called for its side effects and does not return a value.
#'
#' @export
calculatePKParameterForScenarios <- function(projectConfiguration,
                                             scenarioResults) {
  # initialize parameter to avoid linter message
  pKParameter <- scenarioName <- NULL

  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")
  checkmate::assertList(scenarioResults, any.missing = FALSE, names = "named")

  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults)
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  dtScenarios <- getScenarioDefinitions(wbScenarios = projectConfiguration$scenariosFile)
  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
  if (nrow(dtOutputPaths) == 0) stop("Please define ouputPaths in plot configuration xlsx")

  for (sc in names(scenarioResults)) {
    pkParameterSheets <- dtScenarios[scenarioName == sc & !is.na(pKParameter)]$pKParameter
    initializeParametersOfSheets(projectConfiguration, pkParameterSheets)

    if (length(pkParameterSheets) > 0) {
      # Load or calculate PK analyses
      writeToLog(
        type = "Info",
        msg = paste("Calculate  PK analysis result of", sc)
      )

      pkAnalyses <- ospsuite::calculatePKAnalyses(results = scenarioResults[[sc]]$results)

      ospsuite::exportPKAnalysesToCSV(
        pkAnalyses = pkAnalyses,
        filePath = file.path(outputFolder, paste0(sc, ".csv"))
      )
    }
  }

  return(invisible())
}
#' Initialize PK Parameters in Specified Sheets
#'
#' This function initializes pharmacokinetic (PK) parameters in the specified
#' sheets based on user-defined settings. It updates the parameters in-place
#' within the provided PK parameter configuration sheet.
#'
#' @param projectConfiguration A list containing project configuration settings,
#' including the path to the PK parameter configuration sheet
#' @param pkParameterSheets A vector of sheet names that contain the PK parameters
#' to be initialized.
#'
#' @return NULL. The function updates parameters in-place and does not return a value.
#' @keywords internal
initializeParametersOfSheets <- function(projectConfiguration, pkParameterSheets) {
  if (is.null(pkParameterSheets) || length(pkParameterSheets) == 0) {
    return(invisible())
  }

  ospsuite::removeAllUserDefinedPKParameters()

  # Read user-defined PK parameters and clean the names
  dtUserdefPKParameter <- readUserDefinedPKParameters(projectConfiguration$addOns$pKParameterFile)

  # Loop through each PK parameter sheet
  dtPkParameterDefinition <- data.table()
  for (pkParameterSheet in splitInputs(pkParameterSheets)) {
    dtPkParameterDefinition <- xlsxReadData(
      wb = projectConfiguration$addOns$pKParameterFile,
      sheetName = pkParameterSheet,
      skipDescriptionRow = TRUE
    )

    userdefinedParameters <- setdiff(dtPkParameterDefinition$name, ospsuite::allPKParameterNames())
    addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter)
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
#' Read User-Defined Pharmacokinetic (PK) Parameters
#'
#' This function reads user-defined pharmacokinetic (PK) parameters from an Excel
#' file and validates their structure and content.
#'
#' @param file The path to the Excel file containing user-defined PK parameters.
#'
#' @return A data frame of validated user-defined PK parameters.
#' throws Error if validation fails or if required fields are missing.
#' @keywords internal
readUserDefinedPKParameters <- function(file) {
  dtUserdefPKParameter <- xlsxReadData(
    wb = file,
    sheetName = "Userdef PK Parameter",
    skipDescriptionRow = TRUE
  ) %>%
    stats::setNames(gsub(" \\[.*\\]", "", names(.)))

  checkmate::assertCharacter(dtUserdefPKParameter$name, any.missing = FALSE, unique = TRUE)
  if (any(is.na(dtUserdefPKParameter[["display Unit"]]))) {
    stop("empty string is not possible as displayUnit in the sheet 'Userdef PK Parameter',
    workaround: use % and set displayUnit in sheet derived from template-sheet to empty string")
  }

  return(dtUserdefPKParameter)
}
#' Add User-Defined PK Parameters to OSP-Suite
#'
#' This function adds user-defined pharmacokinetic (PK) parameters to OSP-Suite
#' based on the provided definitions. It checks for uniqueness and existence
#' of parameters before adding them.
#'
#' @param userdefinedParameters A vector of user-defined PK parameter names to be added.
#' @param dtUserdefPKParameter A data frame containing definitions for user-defined PK parameters.
#'
#' @return NULL. The function updates the OSPSuite environment and does not return a value.
#' throws Error if user-defined parameters are not defined or are not unique.
#' @export
addUserDefinedParameters <- function(userdefinedParameters, dtUserdefPKParameter) {
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

  return(invisible())
}


# load ----------
#' Load Pharmacokinetic (PK) Parameters for Specified Scenarios
#'
#' This function loads pharmacokinetic (PK) parameters for specified scenarios
#' based on project configuration and a list of scenarios. It processes each
#' scenario and returns the results as a data.table.
#'
#' @param projectConfiguration A list containing project configuration settings,
#' including the PK parameter file.
#' @param scenarioListOrResult A named list of scenarios for which PK parameters
#' are to be loaded.The list should be named and each element must contain
#' an entry simulation, list entries could be e.g. objects of class ScenarioResult or Scenario
#'
#' @return A data.table containing the processed PK analyses for all specified scenarios.
#' @export
#' @param scenarioListOrResult A named list of scenarios for which PK parameters are to be loaded.
#' @return A data.table containing the processed PK analyses.
#'
#' @export
loadPKParameter <- function(projectConfiguration,
                            scenarioListOrResult) {
  # initialize variable to avoid messages
  pKParameter <- scenarioName <- NULL

  checkmate::assertList(scenarioListOrResult, any.missing = FALSE, names = "named", null.ok = TRUE)
  checkmate::assertClass(projectConfiguration, classes = "ProjectConfiguration")

  dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
  if (nrow(dtOutputPaths) == 0) stop("Please define ouputPaths in plot configuration xlsx")
  # Load or calculate PK analyses for all scenarios
  pkAnalysesList <- lapply(names(scenarioListOrResult), function(sc) {
    pkParameterSheets <- dtScenarios[scenarioName == sc & !is.na(pKParameter)]$pKParameter

    loadPKAnalysisPerScenario(
      scenarioName = sc,
      scenarioSimulation = scenarioListOrResult[[sc]]$simulation,
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
#' Load or Calculate PK Analysis for a Given Scenario
#'
#' This function loads PK analysis results from a CSV file for a specific scenario.
#'
#' @param scenarioName The name of the scenario to be processed.
#' @param scenarioSimulation A simulation object corresponding to the scenario.
#' @param pkParameterSheets A vector of sheet names from the PK parameter file to read.
#' @param projectConfiguration A list containing project configuration settings,
#' including the PK parameter file.
#'
#' @return A data.table containing the processed PK analyses for the specified scenario.
#' @keywords internal
loadPKAnalysisPerScenario <- function(scenarioName, scenarioSimulation,
                                      pkParameterSheets, projectConfiguration) {
  # initialize variable to avoid messages
  unitFactor <- value <- NULL

  dtPkAnalyses <- loadPkAnalysisRawData(
    projectConfiguration = projectConfiguration,
    scenarioName = scenarioName,
    scenarioSimulation = scenarioSimulation
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
      scenarioSimulation = scenarioSimulation,
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
            old = c("parameter", "displayName", "displayUnit"),
            new = c("pkParameter", "displayNamePKParameter", "displayUnitPKParameter")
          )
      )
    )
  }

  return(data.table::rbindlist(resultsList))
}
#' Load PK Analysis Results from CSV
#'
#' This function loads PK analysis results from a CSV file or recalculates them
#' if the file is not found. It returns the loaded data as a data.table.
#'
#' @param projectConfiguration A list containing project configuration settings,
#' including the output folder path and the PK parameter file.
#' @param scenarioName The name of the scenario for which PK analysis is to be loaded.
#' @param scenarioSimulation A simulation object corresponding to the scenario.
#'
#' @return A data.table containing the PK analyses loaded from the CSV file.
#' @keywords internal
loadPkAnalysisRawData <- function(projectConfiguration, scenarioName, scenarioSimulation) {
  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults)
  if (!dir.exists(outputFolder)) dir.create(outputFolder)

  fileName <- file.path(outputFolder, paste0(scenarioName, ".csv"))
  if (!file.exists(fileName)) {
    stop(paste(
      "PK Parameter for", scenarioName, "is not calculated!",
      "Use function calculatePKParameterForCalculation to generate the result"
    ))
  }

  writeToLog(
    type = "Info",
    msg = paste("Load PK analysis result of", scenarioName)
  )

  pkAnalyses <- ospsuite::importPKAnalysesFromCSV(
    filePath = fileName,
    simulation = scenarioSimulation
  )

  dtPkAnalyses <- ospsuite::pkAnalysesToDataFrame(pkAnalyses = pkAnalyses) %>%
    data.table::setDT() %>%
    setHeadersToLowerCase()

  dtPkAnalyses[is.na(unit), unit := ""]


  return(dtPkAnalyses)
}
#' Process PK Parameter Definitions for a Scenario
#'
#' This function processes the PK parameter definitions for a specific scenario,
#' including unit conversions and output filters. It ensures that the definitions
#' are aligned with the scenario outputs.
#'
#' @param scenarioSimulation A simulation object corresponding to the scenario.
#' @param dtOutputPaths A data.table containing output paths relevant to the scenario.
#' @param dtPkAnalyses A data.table containing PK analyses to be processed.
#' @param dtPkParameterDefinition A data.table containing definitions of PK parameters.
#'
#' @return A data.table containing processed PK parameter definitions for the scenario.
#' @keywords internal
addUnitFactorsToPKDefinition <- function(scenarioSimulation,
                                         dtOutputPaths,
                                         dtPkAnalyses,
                                         dtPkParameterDefinition) {
  # Initialize variables to avoid linter messages
  outputPathIds <- molweight <- outputPath <- NULL

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
    dtO[outputPath == pt, molweight := scenarioSimulation$molWeightFor(pt)]
  }

  # Remove descriptions from dtPkParameterDefinition and handle NA outputPathIds
  dtPkParameterDefinition <- dtPkParameterDefinition %>%
    dplyr::select(!dplyr::any_of(c("descriptions")))

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

  if (nrow(dtPkParameterDefinition) == 0) {
    return(dtPkParameterDefinition)
  }

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
# plot Support --------
#' Merge PK Parameters with Configuration Table
#'
#' This function merges pharmacokinetic (PK) parameters with a given plot configuration
#' table. It processes the configuration data to ensure compatibility with the PK parameter
#' data and prepares the data for visualization or further analysis.
#'
#' @param onePlotConfig A data.table containing plot configuration settings, including
#'                      scenario names, PK parameters, and output path IDs.
#' @param pkParameterDT A data.table containing pharmacokinetic parameters, including
#'                      scenario names, parameters, individual IDs, values, and
#'                      output path IDs.
#' @param colorVector An optional vector specifying colors for different scenarios.
#'                    If provided, it will be used to differentiate between reference
#'                    and non-reference scenarios in the merged data.
#' @param asRatio A logical value indicating whether to convert values to ratios
#'                 between reference and base scenarios. Defaults to FALSE.
#'
#' @return A data.table containing the merged data with PK parameters and configuration
#'         settings, including any calculated ratios if `asRatio` is TRUE.
#'
#' @keywords internal
mergePKParameterWithConfigTable <- function(onePlotConfig,
                                            pkParameterDT,
                                            colorVector = NULL,
                                            asRatio = FALSE) {
  # initialize to avoid linter messages
  isReference <- isReference <- colorIndex <- referenceScenario <- scenario <- NULL

  onePlotConfig <- data.table::copy(onePlotConfig)
  for (col in intersect(
    names(onePlotConfig),
    c("scenarios", "pkParameters", "outputPathIds")
  )) {
    onePlotConfig <- separateAndTrim(onePlotConfig, col)
  }
  mergedData <- onePlotConfig %>%
    dplyr::select(dplyr::any_of(c(
      "plotName", "scenario", "referenceScenario", "pkParameter", "outputPathId",
      "scenarioShortName", "scenarioLongName"
    ))) %>%
    merge(
      pkParameterDT %>%
        unique(),
      by = c("scenario", "pkParameter", "outputPathId")
    ) %>%
    merge(configEnv$outputPaths[, c("outputPathId", "displayNameOutput")],
      by = "outputPathId"
    )

  if (nrow(mergedData) == 0) stop(paste("no PK-Parameter available for", onePlotConfig$plotName[1]))


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

  mergedData$scenarioShortName <- factor(
    mergedData$
      scenarioShortName,
    levels = unique(onePlotConfig$scenarioShortName),
    ordered = TRUE
  )

  mergedData$scenarioLongName <- factor(
    mergedData$
      scenarioLongName,
    levels = unique(onePlotConfig$scenarioLongName),
    ordered = TRUE
  )

  return(mergedData)
}
#' Compute the Ratio of Values Between Base and Reference Scenarios
#'
#' This function calculates the ratio of values between base and reference scenarios
#' for specified pharmacokinetic parameters. It merges the configuration data with
#' pharmacokinetic parameter data and computes the ratio based on individual IDs
#' and output path IDs.
#'
#' @param mergedData A data.table containing configuration data with columns for
#'                   referenceScenario, pkParameter, individualId, and outputPathId.
#' @param pkParameterDT A data.table containing pharmacokinetic parameter data
#'                      including scenario names, parameters, individual IDs,
#'                      output path IDs, values, and population IDs.
#'
#' @return A data.table containing the merged data with a new column `value`
#'         representing the ratio of base to reference values.
#' @keywords internal
setValueToRatio <- function(mergedData, pkParameterDT) {
  # initialize variable to avoid messages
  valueBase <- valueReference <- value <- NULL

  mergedData <- merge(mergedData,
    pkParameterDT[, c("scenario", "pkParameter", "individualId", "outputPathId", "value", "populationId")] %>%
      setnames(
        old = c("scenario"),
        new = c("referenceScenario")
      ),
    by = c("referenceScenario", "pkParameter", "individualId", "outputPathId"),
    suffixes = c("Base", "Reference")
  )
  mergedData[, value := valueBase / valueReference]


  return(mergedData)
}
#' Validate Structure and Content of PK Parameter Data Table
#'
#' This function validates the structure and content of the provided
#' pharmacokinetic parameter data table (`pkParameterDT`). It checks
#' for the presence of required columns and ensures that the combination
#' of `outputPathId` and `parameter` is unique with consistent
#' `displayUnitPKParameter`.
#'
#' @param pkParameterDT A data.table containing pharmacokinetic parameters
#'                      with the required columns: scenario, pkParameter,
#'                      individualId, value, outputPathId, displayNamePKParameter,
#'                      and displayUnitPKParameter.
#'
#' @return NULL. The function will stop execution if validation fails,
#'         otherwise returns invisibly.
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
