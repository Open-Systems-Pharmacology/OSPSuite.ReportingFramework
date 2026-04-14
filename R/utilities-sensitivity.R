#' Add Sensitivity Table to Project Configuration
#'
#' This function adds a sensitivity table to the provided project configuration.
#' It loads a template Excel file and populates it with parameter paths from the specified scenario.
#'
#' @param projectConfiguration An object representing the project configuration.
#' @param scenarioList A list of scenarios from which to extract parameter paths. Defaults to NULL.
#' @param scenarioName The name of the scenario to extract parameter paths from.
#' @param sheetName The name of the sheet in the Excel file to populate. Defaults to the value of scenarioName.
#'
#' @return The modified project configuration object.
#' @export
addSensitivityTable <- function(projectConfiguration, scenarioList = NULL, scenarioName, sheetName = scenarioName) {
  invisible(projectConfiguration$addAddOnFileToConfiguration(
    property = "sensitivityFile",
    value = "SensitivityParameter.xlsx",
    description = "Configuration file for Sensitivity",
    templatePath = system.file("templates", "SensitivityParameter.xlsx", package = "ospsuite.reportingframework")
  ))

  if (!is.null(scenarioList)) {
    checkmate::assertChoice(scenarioName, choices = names(scenarioList))
    checkmate::assertString(sheetName, max.chars = 31)

    parameterPaths <- ospsuite::potentialVariableParameterPathsFor(scenarioList[[scenarioName]]$simulation)

    dt <- data.table(
      parameter = gsub("\\|", " ", parameterPaths),
      parameterPath = parameterPaths
    )

    wb <- xlsxAddDataUsingTemplate(
      wb = openxlsx::loadWorkbook(projectConfiguration$addOns$sensitivityFile),
      templateSheet = "Template",
      sheetName = sheetName,
      dtNewData = dt
    )
    openxlsx::saveWorkbook(wb, projectConfiguration$addOns$sensitivityFile, overwrite = TRUE)
  }

  return(projectConfiguration)
}
#' Run Sensitivity Analysis for Specified Scenarios
#'
#' This function executes sensitivity analysis for the specified scenarios in the project configuration.
#' It checks for the presence of the sensitivity file and runs the analysis, saving the results to CSV files.
#'
#' @param projectConfiguration An object representing the project configuration.
#' @param scenarioList A list of scenarios to analyze.
#' @param scenarioNames A vector of scenario names to run the analysis on. Defaults to NULL.
#' @param sensitivitysheet The name of the sheet in the sensitivity Excel file to use for analysis.
#' @param sensitivityAnalysisRunOptions Options for running the sensitivity analysis.
#' Defaults to an instance of `SensitivityAnalysisRunOptions` with `showProgress` set to TRUE.
#' @param overwrite A logical indicating whether to overwrite existing results. Defaults to TRUE.
#'
#' @return NULL
#' @export
runSensitivityAnalysisForScenarios <-
  function(projectConfiguration,
           scenarioList,
           scenarioNames = NULL,
           sensitivitysheet,
           sensitivityAnalysisRunOptions = ospsuite::SensitivityAnalysisRunOptions$new(showProgress = TRUE),
           overwrite = TRUE) {
    # initialize variable to avoid messages
    pKParameter <- NULL

    if (!("sensitivityFile" %in% names(projectConfiguration$addOns))) {
      stop(messages$errorSensitivityFileNotAdded())
    }

    outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$sensitivityResults)
    if (!dir.exists(outputFolder)) dir.create(outputFolder)

    dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

    sensitivityParameterDt <- xlsxReadData(projectConfiguration$addOns$sensitivityFile, sheetName = sensitivitysheet)

    for (scenarioName in scenarioNames) {
      if (!file.exists(file.path(outputFolder, .sensitivityAnalyisName(scenarioName, sensitivitysheet))) |
        overwrite) {
        pkParameterSheets <- dtScenarios[scenarioName == scenarioName & !is.na(pKParameter)]$pKParameter
        if (length(pkParameterSheets) > 0) {
          .initializeParametersOfSheets(projectConfiguration, pkParameterSheets)

          # Get scenario configuration
          scenarioConfig <- dtScenarios[scenarioName == scenarioName]
          
          # Get model file path
          modelFile <- file.path(projectConfiguration$modelFolder, scenarioConfig$modelFile[1])
          
          # Get output paths from configuration
          dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
          outputPaths <- unique(dtOutputPaths$quantityPath)
          
          # Get PK parameters to calculate
          dtPKParams <- xlsxReadData(projectConfiguration$scenariosFile, sheetName = pkParameterSheets[1])
          pkParameters <- unique(dtPKParams$pKParameter)
          
          # Convert sensitivity parameter data.table to named list format
          # Use parameter column as names, parameterPath as single-element vectors
          sensitivityParameter <- as.list(sensitivityParameterDt$parameterPath)
          names(sensitivityParameter) <- sensitivityParameterDt$parameter
          
          # Call calculateSensitivities
          scenarioFiles <- stats::setNames(modelFile, scenarioName)
          
          calculateSensitivities(
            scenarioFiles = scenarioFiles,
            outputPaths = outputPaths,
            pkParameter = pkParameters,
            sensitivityParameter = sensitivityParameter,
            outFolder = projectConfiguration$outputFolder,
            simulationRunOptions = sensitivityAnalysisRunOptions
          )
        }
      }
    }
  }
#' Generate Sensitivity Analysis Output File Name
#'
#' This function generates a name for the sensitivity analysis output file based on the scenario name and sheet name.
#'
#' @param scenarioName The name of the scenario.
#' @param sensitivitysheet The name of the sensitivity sheet.
#'
#' @return A string representing the generated file name for the sensitivity analysis results.
#' @keywords internal
#' @noRd
.sensitivityAnalyisName <- function(scenarioName, sensitivitysheet) {
  paste0(scenarioName, "_", sensitivitysheet, ".csv")
}

#' Calculate sensitivities of PK parameters (and optional ratio between two scenarios)
#'
#' Calculate the sensitivity of pharmacokinetic (PK) parameters with respect to one or more
#' model parameter paths.
#' The function runs a set of simulations for factor-perturbed parameter sets,
#' computes sensitivities (finite-difference slope), and writes sensitivity results to CSV.
#'
#' If exactly two scenarios are supplied in scenarioFiles, the PK values used for sensitivity
#' calculation are the ratio of scenario2 / scenario1 (useful for e.g. DDI analyses where
#' scenario 1 = control and scenario 2 = treatment).
#'
#' @details
#' Background and limitations (default ospsuite method):
#' The default sensitivity method implemented in ospsuite's SensitivityAnalysis computes
#' sensitivities per model parameter and assumes a one-to-one mapping between the
#' reported "sensitivity parameter" and an underlying model parameter path.
#' - That default method does not support ratio mode (i.e., calculating sensitivities
#' of scenario2 / scenario1).
#' - The default method does not support a single sensitivity parameter mapped to more
#' than one model parameter path (many-to-one mapping).
#'
#'  What this function documents / does:
#'  This function was developed to enhance the default ospsuite calculation for single-parameter sensitivities,
#'  and to provide a clear and reproducible CSV output. The function can be used in an extended
#'  mode to compute sensitivities on ratios (scenario2/scenario1) and to compute sensitivities for a named sensitivity
#'  parameter that maps to multiple model parameter paths — but you must pass the appropriate scenarioFiles
#'  and sensitivityParameter arguments.
#'  The implementation:  builds perturbed populations by multiplicatively varying every model parameter
#'  path associated with a named sensitivity parameter, and computes finite-difference sensitivities using the
#'  baseline factor (= 1) rows as reference.
#'
#' Side effects:
#' - Writes PK analysis CSVs for each scenario into a subfolder "<outFolder>/SensitivityResults".
#' - Writes a final "<last_scenario_name>-sensitivity.csv" file into the same folder.
#'
#' @param scenarioFiles Named character vector or named list of file paths to simulation (PKML) files.
#'   Names are used to form output filenames; length must be 1 or 2.
#' @param outputPaths Character vector of QuantityPath values to keep from PK analysis output.
#' @param pkParameter Character vector of PK parameter names to use.
#' @param sensitivityParameter Named list where each element name is a sensitivity parameter label
#'   and each element is a character vector of parameter paths to be varied for that label.
#'   Example: list("Lipophilicity" = c("Aciclovir|Lipophilicity"),
#'   "Permeability Mucosa" = c("Neighborhoods|Duodenum_int_Duodenum_cell|Aciclovir|P (interstitial->intracellular)",
#'                             "Neighborhoods|Duodenum_int_Duodenum_cell|Aciclovir|P (intracellular->interstitial)")).
#' @param variationRange Numeric scalar (default 0.1). The fractional variation range (e.g., 0.1 =
#'   ±10% around baseline).
#' @param numberOfSteps Integer (default 2). Number of positive steps between baseline and
#'   +variationRange; both the factors >1 and their reciprocals <1 are used.
#' @param outFolder Character scalar. Base folder where the sensitivity results and temporary
#'   PK analysis files will be written. A subfolder "SensitivityResults" will be created.
#' @param simulationRunOptions SimulationRunOptions object passed to the simulation runner (optional).
#'
#' @return Invisibly returns a data.table with the computed sensitivity values. The data.table has
#'   columns at least: SensitivityParameter, QuantityPath, PKParameter, sens (aggregate sensitivity).
#'   Primary outputs are also written to CSV files as described above.
#'
#' @examples
#' \dontrun{
#' calculateSensitivities(scenarioFiles = myScenarioFiles,
#'                        outputPaths = c("/Plasma/Drug"),
#'                        pkParameter = c("AUC"),
#'                        sensitivityParameter = list("CL" = c("/Parameters/CL")),
#'                        outFolder = "results")
#' }
#' @export
calculateSensitivities <- function(scenarioFiles,
                                   outputPaths,
                                   pkParameter,
                                   sensitivityParameter,
                                   variationRange = 0.1,
                                   numberOfSteps = 2,
                                   outFolder = tempdir(),
                                   simulationRunOptions = NULL) {

  checkmate::assertCharacter(scenarioFiles, min.len = 1, max.len = 2, any.missing = FALSE, names = "named")
  checkmate::assertCharacter(outputPaths, any.missing = FALSE)
  checkmate::assertCharacter(pkParameter, any.missing = FALSE)
  checkmate::assertList(sensitivityParameter, min.len = 1)
  checkmate::assertNumber(variationRange, lower = 0)
  checkmate::assertInt(numberOfSteps, lower = 1)
  checkmate::assertString(outFolder)

  # Validate files exist
  for (fp in scenarioFiles) {
    if (!file.exists(fp)) stop(sprintf("Simulation file not found: %s", fp))
  }

  # Create output folder for sensitivity results
  sensitivityFolder <- file.path(outFolder, "SensitivityResults")
  if (!dir.exists(sensitivityFolder)) dir.create(sensitivityFolder, recursive = TRUE)

  # Prepare temporary sensitivity population (this function validates baseline parameter consistency)
  sensitivityPopulation <- .prepareSensitivityPopulation(
    scenarioFiles = scenarioFiles,
    sensitivityParameter = sensitivityParameter,
    variationRange = variationRange,
    numberOfSteps = numberOfSteps
  )

  # Run simulations for each scenario and export PK analysis CSV
  for (scenarioName in names(scenarioFiles)) {
    simfile <- scenarioFiles[[scenarioName]]
    simObj <- ospsuite::loadSimulation(filePath = simfile)

    # runSimulations may be provided by ospsuite or wrapper; pass simulationRunOptions through
    scenarioResults <- ospsuite::runSimulations(
      simulations = simObj,
      population = sensitivityPopulation,
      simulationRunOptions = simulationRunOptions
    )

    # take first result set (runSimulations returns list; this mirrors original behavior)
    if (length(scenarioResults) < 1) stop("runSimulations returned no results")
    pkanalyses <- ospsuite::calculatePKAnalyses(scenarioResults[[1]])

    ospsuite::exportPKAnalysesToCSV(
      pkAnalyses = pkanalyses,
      filePath = file.path(sensitivityFolder, paste0(scenarioName, "-PKAnalysisResults.csv"))
    )
  }

  # Convert population to data.frame/data.table and select needed columns
  sensitivityPopulationDF <- ospsuite::populationToDataFrame(sensitivityPopulation)
  data.table::setDT(sensitivityPopulationDF)
  # Expect columns: IndividualId, factor, SensitivityParameter, plus parameter path columns
  if (!all(c("IndividualId", "factor", "SensitivityParameter") %in% names(sensitivityPopulationDF))) {
    stop("Loaded population does not contain required columns: IndividualId, factor, SensitivityParameter")
  }
  sensitivityPopulationDT <- sensitivityPopulationDF[, .(IndividualId, factor, SensitivityParameter)]

  # Load PK values: returns data.table with columns QuantityPath, PKParameter, Value, IndividualId
  pkVals <- .loadPKValues(
    scenarioFiles = scenarioFiles,
    outputPaths = outputPaths,
    pkParameter = pkParameter,
    outFolder = sensitivityFolder
  )

  # Merge population table with PK values by IndividualId
  data.table::setDT(pkVals)
  pkParameterDTSens <- merge(sensitivityPopulationDT, pkVals, by = "IndividualId", sort = FALSE)

  # Ensure factor numeric
  pkParameterDTSens[, factor := as.numeric(factor)]

  # Create baseline merge: find rows with factor == 1 and merge to others using QuantityPath and PKParameter
  baseline <- pkParameterDTSens[factor == 1, .(QuantityPath, PKParameter, Value.base = Value)]
  nonbase <- pkParameterDTSens[factor != 1]
  pkParameterDTSens <- merge(nonbase, baseline, by = c("QuantityPath", "PKParameter"), all.x = TRUE, sort = FALSE)

  if (any(is.na(pkParameterDTSens$Value.base))) {
    stop("Some perturbed rows do not have matching baseline rows. Check that baseline factor==1 rows exist for all QuantityPath / PKParameter combinations.")
  }

  # Compute fractional changes and sensitivities per (SensitivityParameter, QuantityPath, PKParameter)
  pkParameterDTSens[, dPK := (Value - Value.base) / Value.base]
  pkParameterDTSens[, dP := (factor - 1)]
  pkParameterDTSens[, sens := dPK / dP]

  # Aggregate sensitivity (average over perturbation rows)
  sens <- pkParameterDTSens[, .(sens = mean(sens, na.rm = TRUE)),
    by = .(SensitivityParameter, QuantityPath, PKParameter)
  ]

  # Write out CSV using the last scenario name
  lastScenarioName <- names(scenarioFiles)[length(scenarioFiles)]
  utils::write.csv(
    x = sens,
    file = file.path(sensitivityFolder, paste0(lastScenarioName, "-sensitivity.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  return(invisible(sens))
}


#' Load PK analysis results (and optional ratio if two scenarios present)
#'
#' Load PK analysis results from CSV files written by ospsuite::exportPKAnalysesToCSV and
#' optionally compute the ratio of values between two scenarios (scenario2 / scenario1).
#'
#' The function expects files named "<scenarioName>-PKAnalysisResults.csv" to exist in `outFolder`.
#'
#' @param scenarioFiles Named character vector/list of scenario file names (used for naming).
#' @param outputPaths Character vector of QuantityPath values to keep from the PK analysis CSV.
#' @param pkParameter Character vector of parameter names to keep (e.g., c("AUC_day13")).
#' @param outFolder Character scalar. Folder that contains "<scenarioName>-PKAnalysisResults.csv".
#'
#' @return A data.table containing the selected PK analysis rows. If two scenarios were supplied,
#'   the Value column contains `scenario2 / scenario1` for each matching QuantityPath and Parameter.
#'   The returned data.table will have a column `PKParameter` (renamed from `Parameter`).
#'
#' @keywords internal
#' @noRd
.loadPKValues <- function(scenarioFiles,
                         outputPaths,
                         pkParameter,
                         outFolder) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' required")
  if (!requireNamespace("utils", quietly = TRUE)) stop("Package 'utils' required")

  checkmate::assertCharacter(scenarioFiles, min.len = 1, max.len = 2)
  checkmate::assertCharacter(outputPaths, any.missing = FALSE)
  checkmate::assertCharacter(pkParameter, any.missing = FALSE)
  checkmate::assertString(outFolder)

  # Load reference scenario CSV
  refFile <- file.path(outFolder, paste0(names(scenarioFiles)[1], "-PKAnalysisResults.csv"))
  if (!file.exists(refFile)) stop(sprintf("PK analysis file not found: %s", refFile))
  dtPK <- data.table::fread(file = refFile)

  # If two scenarios are provided, compute ratio scenario2 / scenario1
  if (length(scenarioFiles) == 2) {
    numFile <- file.path(outFolder, paste0(names(scenarioFiles)[2], "-PKAnalysisResults.csv"))
    if (!file.exists(numFile)) stop(sprintf("PK analysis file not found: %s", numFile))
    dtNumerator <- data.table::fread(file = numFile)

    # Merge on all columns except Value and Value.* variants; conservatively merge by key columns
    keyCols <- intersect(names(dtPK), names(dtNumerator))
    keyCols <- setdiff(keyCols, "Value")
    if (length(keyCols) == 0) stop("No common columns to merge numerator and reference PK result files")
    dtPK <- merge(dtPK, dtNumerator, by = keyCols, suffixes = c(".reference", ".numerator"))

    # Expect columns Value.reference and Value.numerator
    if (!("Value.reference" %in% names(dtPK)) || !("Value.numerator" %in% names(dtPK))) {
      # fallback to Value.reference / Value.numerator naming if different
      if ("Value.reference" %in% names(dtPK) && "Value.numerator" %in% names(dtPK)) {
        # ok
      } else {
        # try to detect Value.* columns
        valueCols <- grep("^Value", names(dtPK), value = TRUE)
        if (length(valueCols) >= 2) {
          # use the two found
          data.table::setnames(dtPK, old = valueCols[1], new = "Value.reference")
          data.table::setnames(dtPK, old = valueCols[2], new = "Value.numerator")
        } else {
          stop("Could not find Value columns after merging numerator and reference PK result files")
        }
      }
    }

    dtPK[, Value := Value.numerator / Value.reference]
  }

  # Filter by requested QuantityPath and Parameter (Parameter will be renamed to PKParameter)
  if (!("QuantityPath" %in% names(dtPK))) stop("Input PK-analysis CSV does not contain 'QuantityPath' column")
  if (!("Parameter" %in% names(dtPK))) stop("Input PK-analysis CSV does not contain 'Parameter' column")

  dtPK <- dtPK[QuantityPath %in% outputPaths]
  dtPK <- dtPK[Parameter %in% pkParameter]

  data.table::setnames(dtPK, old = c("Parameter"), new = c("PKParameter"))

  # Ensure IndividualId exists (if not present assume single individual 0)
  if (!("IndividualId" %in% names(dtPK))) {
    dtPK[, IndividualId := 0]
  }

  # Keep only needed columns
  keepCols <- intersect(c("IndividualId", "QuantityPath", "PKParameter", "Value"), names(dtPK))
  dtPK <- dtPK[, ..keepCols]

  return(dtPK)
}


#' Prepare a population object for sensitivity analysis
#'
#' Create a population (ospsuite population object) that contains baseline and perturbed parameter
#' entries for sensitivity analysis. For each named element in `sensitivityParameter`, multiple
#' rows are generated using multiplicative factors (both >1 and reciprocals <1). The function
#' returns an ospsuite population object that can be passed to the simulation runner.
#'
#' @param scenarioFiles Named character vector/list of scenario file paths (length >= 1).
#'   to obtain baseline parameter values.
#' @param sensitivityParameter Named list of sensitivity parameter groups. Each element name is
#'   used as the `SensitivityParameter` label and each element is a character vector of parameter
#'   paths to be multiplied.
#' @param variationRange Numeric (default 0.1). Fractional change range for the positive steps.
#' @param numberOfSteps Integer (default 2). Number of positive steps; factors used will be
#'   1 + variationRange * (i/numberOfSteps) for i = 1..numberOfSteps and their reciprocals.
#'
#' @return An ospsuite population object (loaded via ospsuite::loadPopulation) containing baseline
#'   and perturbed parameter sets. The returned population includes a column `IndividualId`
#'   enumerating rows from 0.
#'
#' @keywords internal
#' @noRd
.prepareSensitivityPopulation <- function(scenarioFiles,
                                          sensitivityParameter,
                                          variationRange = 0.1,
                                          numberOfSteps = 2) {

  checkmate::assertCharacter(scenarioFiles, min.len = 1, max.len = 2)
  checkmate::assertList(sensitivityParameter, min.len = 1)
  checkmate::assertNumber(variationRange, lower = 0)
  checkmate::assertInt(numberOfSteps, lower = 1)

  popFileName <- file.path(tempdir(), "Sensitivity-population.csv")

  # multiplicative factors: positive steps and their reciprocals, plus baseline 1
  positive <- 1 + variationRange * (seq_len(numberOfSteps) / numberOfSteps)
  factors <- sort(unique(c(positive, 1 / positive, 1)))

  # Flatten sensitivity paths
  sensitivityPaths <- unique(unlist(sensitivityParameter))

  # Load baseline values from the first scenario file (fresh load)
  firstSimFile <- scenarioFiles[[1]]
  if (!file.exists(firstSimFile)) stop(sprintf("First scenario file not found: %s", firstSimFile))
  sim1 <- ospsuite::loadSimulation(filePath = firstSimFile, loadFromCache = FALSE)

  # Robust getter for parameters: return NULL if not found or any error occurs
  getParValue <- function(parPath, container) {
    res <- ospsuite::getParameter(path = parPath, container = container, stopIfNotFound = FALSE)
    # If result exists but has no value field, treat as not found
    if (is.null(res)) {
      return(NULL)
    }
    if (!("value" %in% names(res))) {
      return(NULL)
    }
    return(res)
  }

  baselineValues <- list()
  missingPaths <- character(0)
  for (parPath in sensitivityPaths) {
    res <- getParValue(parPath, sim1)
    if (is.null(res)) {
      warning(sprintf("Parameter path not found in first scenario '%s': %s", firstSimFile, parPath), call. = FALSE)
      missingPaths <- c(missingPaths, parPath)
      baselineValues[[parPath]] <- NA_real_
    } else {
      baselineValues[[parPath]] <- as.numeric(res$value)
    }
  }

  # If second scenario present, validate equality for those paths (if present there)
  if (length(scenarioFiles) == 2) {
    secondSimFile <- scenarioFiles[[2]]
    if (!file.exists(secondSimFile)) stop(sprintf("Second scenario file not found: %s", secondSimFile))
    sim2 <- ospsuite::loadSimulation(filePath = secondSimFile, loadFromCache = FALSE)

    for (parPath in sensitivityPaths) {
      res2 <- getParValue(parPath, sim2)
      if (is.null(res2)) {
        warning(sprintf("Parameter path not found in second scenario '%s': %s", secondSimFile, parPath), call. = FALSE)
        # leave baselineValues as-is (NA or value from first)
      } else {
        val2 <- as.numeric(res2$value)
        val1 <- baselineValues[[parPath]]
        # If val1 is NA (not found in first but found in second), adopt val2
        if (is.na(val1) && !is.na(val2)) {
          baselineValues[[parPath]] <- val2
          # also remove from missingPaths if it was previously missing
          missingPaths <- setdiff(missingPaths, parPath)
        } else if (!is.na(val1) && !is.na(val2)) {
          # Check for zero values in either scenario
          if ((val1 == 0 && val2 != 0) || (val2 == 0 && val1 != 0)) {
            stop(sprintf("Baseline parameter value mismatch for '%s' between scenarios: %g vs %g", parPath, val1, val2))
          }
          # Only perform division check if val1 is not zero
          if (val1 != 0 && !isTRUE(all.equal(val2 / val1, 1, tolerance = 1e-8))) {
            stop(sprintf("Baseline parameter value mismatch for '%s' between scenarios: %g vs %g", parPath, val1, val2))
          }
          # If both are zero, they match (no error)
        }
        # if both NA, keep NA and let missingPaths reflect that
      }
    }
  }

  # Determine which paths are actually available (non-NA baseline)
  availablePaths <- names(baselineValues)[!is.na(unlist(baselineValues))]
  if (length(availablePaths) == 0) {
    stop("None of the requested sensitivity parameter paths were found in the provided scenario(s). Aborting.")
  }

  # Filter sensitivityParameter groups to only include available paths; warn about removed ones
  filteredSensitivityParameter <- list()
  for (sensName in names(sensitivityParameter)) {
    paths <- sensitivityParameter[[sensName]]
    kept <- paths[paths %in% availablePaths]
    removed <- setdiff(paths, kept)
    if (length(removed) > 0) {
      warning(sprintf(
        "For sensitivity parameter '%s', the following paths were not found and will be ignored: %s",
        sensName, paste(removed, collapse = ", ")
      ), call. = FALSE)
    }
    if (length(kept) > 0) {
      filteredSensitivityParameter[[sensName]] <- kept
    } else {
      warning(sprintf("Sensitivity parameter '%s' has no valid parameter paths after filtering and will be skipped", sensName),
        call. = FALSE
      )
    }
  }
  if (length(filteredSensitivityParameter) == 0) {
    stop("After filtering missing parameter paths, no sensitivity parameters remain. Aborting.")
  }

  # Keep baselineValues only for availablePaths
  baselineValues <- baselineValues[availablePaths]

  # Build default population row using baseline values
  populationDefault <- as.data.table(as.list(baselineValues))
  populationDefault[, factor := 1]
  populationDefault[, SensitivityParameter := ""]

  # Create sensitivity data for each named sensitivity parameter (label)
  popPerParList <- lapply(names(filteredSensitivityParameter), function(sensPar) {
    pathList <- filteredSensitivityParameter[[sensPar]]
    tmpList <- lapply(factors[factors != 1], function(f) {
      modifiedPopulation <- data.table::copy(populationDefault)
      for (parPath in pathList) {
        if (!(parPath %in% names(modifiedPopulation))) {
          stop(sprintf("Parameter path '%s' not present in population default (this should not happen)", parPath))
        }
        modifiedPopulation[, (parPath) := as.numeric(get(parPath)) * f]
      }
      modifiedPopulation[, factor := f]
      modifiedPopulation[, SensitivityParameter := sensPar]
      modifiedPopulation
    })
    data.table::rbindlist(tmpList, use.names = TRUE, fill = TRUE)
  })

  finalPopulation <- data.table::rbindlist(c(list(populationDefault), popPerParList), use.names = TRUE, fill = TRUE)
  finalPopulation[, IndividualId := .I - 1]
  data.table::setcolorder(finalPopulation, c("IndividualId"))

  # Write to temp CSV and reload as ospsuite population object
  utils::write.csv(finalPopulation, file = popFileName, row.names = FALSE, fileEncoding = "UTF-8")
  popObj <- ospsuite::loadPopulation(popFileName)

  return(popObj)
}
