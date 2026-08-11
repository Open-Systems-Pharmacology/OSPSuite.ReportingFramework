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
addSensitivityTable <- function(
  projectConfiguration,
  scenarioList = NULL,
  scenarioName,
  sheetName = scenarioName
) {
  invisible(projectConfiguration$addAddOnFileToConfiguration(
    property = "sensitivityFile",
    value = "SensitivityParameter.xlsx",
    description = "Configuration file for Sensitivity",
    templatePath = system.file(
      "templates",
      "SensitivityParameter.xlsx",
      package = "ospsuite.reportingframework"
    )
  ))

  if (!is.null(scenarioList)) {
    checkmate::assertChoice(scenarioName, choices = names(scenarioList))
    checkmate::assertString(sheetName, max.chars = 31)

    parameterPaths <- ospsuite::potentialVariableParameterPathsFor(
      scenarioList[[scenarioName]]$simulation
    )

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
    openxlsx::saveWorkbook(
      wb,
      projectConfiguration$addOns$sensitivityFile,
      overwrite = TRUE
    )
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
  function(
    projectConfiguration,
    scenarioList,
    scenarioNames = NULL,
    sensitivitysheet,
    sensitivityAnalysisRunOptions = ospsuite::SensitivityAnalysisRunOptions$new(
      showProgress = TRUE
    ),
    overwrite = TRUE
  ) {
    # initialize variable to avoid messages
    pKParameter <- NULL

    if (!("sensitivityFile" %in% names(projectConfiguration$addOns))) {
      stop(messages$errorutilitiessensitivityL1())
    }

    outputFolder <- file.path(
      projectConfiguration$outputFolder,
      EXPORTDIR$sensitivityResults
    )
    if (!dir.exists(outputFolder)) {
      dir.create(outputFolder)
    }

    dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

    sensitivityParameterDt <- xlsxReadData(
      projectConfiguration$addOns$sensitivityFile,
      sheetName = sensitivitysheet
    )

    for (scenarioName in scenarioNames) {
      if (
        !file.exists(file.path(
          outputFolder,
          sensitivityAnalysisName(scenarioName, sensitivitysheet)
        )) |
          overwrite
      ) {
        pkParameterSheets <- dtScenarios[
          scenarioName == scenarioName & !is.na(pKParameter)
        ]$pKParameter
        if (length(pkParameterSheets) > 0) {
          .initializeParametersOfSheets(projectConfiguration, pkParameterSheets)

          sensitivityAnalysis <- ospsuite::SensitivityAnalysis$new(
            simulation = scenarioList[[scenarioName]]$simulation,
            parameterPaths = sensitivityParameterDt$parameterPath
          )

          sensitivityResults <-
            ospsuite::runSensitivityAnalysis(
              sensitivityAnalysis = sensitivityAnalysis,
              sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
            )

          ospsuite::exportSensitivityAnalysisResultsToCSV(
            results = sensitivityResults,
            filePath = file.path(
              outputFolder,
              sensitivityAnalysisName(scenarioName, sensitivitysheet)
            )
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
sensitivityAnalysisName <- function(scenarioName, sensitivitysheet) {
  paste0(scenarioName, "_", sensitivitysheet, ".csv")
}

# custom sensitivity (DDI ratio mode + many-to-one parameter mappings) ----

#' Calculate sensitivities of PK parameters with optional DDI ratio mode
#'
#' Extends the built-in ospsuite sensitivity analysis to support:
#' - **DDI ratio mode**: when two scenarios are supplied, sensitivities are
#'   computed on the ratio scenario2 / scenario1 (e.g. treatment / control).
#' - **Many-to-one mappings**: a single sensitivity label can map to multiple
#'   model parameter paths that are varied simultaneously.
#'
#' Writes one `<scenarioName>-PKAnalysisResults.csv` per scenario and a final
#' `<lastScenarioName>-sensitivity.csv` into `<outFolder>/SensitivityResults/`.
#'
#' @param scenarioFiles Named character vector (length 1 or 2) of PKML file paths.
#'   Names become output file prefixes.
#' @param outputPaths Character vector of `QuantityPath` values to keep.
#' @param pkParameter Character vector of PK parameter names to use.
#' @param sensitivityParameter Named list; each element name is a sensitivity
#'   label and each element is a character vector of model parameter paths
#'   to vary for that label.
#' @param variationRange Numeric fraction variation range (default `0.1` = ±10%).
#' @param numberOfSteps Integer number of positive perturbation steps (default `2`).
#' @param outFolder Base output folder; a `SensitivityResults/` subfolder is created.
#' @param simulationRunOptions Optional `SimulationRunOptions` passed to ospsuite.
#'
#' @return Invisibly returns a data.table with columns
#'   `SensitivityParameter`, `QuantityPath`, `PKParameter`, `sens`.
#' @export
#' @family sensitivity functions
calculateSensitivities <- function(
  scenarioFiles,
  outputPaths,
  pkParameter,
  sensitivityParameter,
  variationRange = 0.1,
  numberOfSteps = 2,
  outFolder = tempdir(),
  simulationRunOptions = NULL
) {
  checkmate::assertCharacter(
    scenarioFiles,
    min.len = 1,
    max.len = 2,
    any.missing = FALSE,
    names = "named"
  )
  checkmate::assertCharacter(outputPaths, any.missing = FALSE)
  checkmate::assertCharacter(pkParameter, any.missing = FALSE)
  checkmate::assertList(sensitivityParameter, min.len = 1)
  checkmate::assertNumber(variationRange, lower = 0)
  checkmate::assertInt(numberOfSteps, lower = 1)
  checkmate::assertString(outFolder)
  for (fp in scenarioFiles) {
    checkmate::assertFileExists(fp, .var.name = paste("scenarioFile", fp))
  }

  sensitivityFolder <- file.path(outFolder, EXPORTDIR$sensitivityResults)
  if (!dir.exists(sensitivityFolder)) {
    dir.create(sensitivityFolder, recursive = TRUE)
  }

  sensitivityPopulation <- prepareSensitivityPopulation(
    scenarioFiles = scenarioFiles,
    sensitivityParameter = sensitivityParameter,
    variationRange = variationRange,
    numberOfSteps = numberOfSteps
  )

  for (scenarioName in names(scenarioFiles)) {
    simObj <- ospsuite::loadSimulation(filePath = scenarioFiles[[scenarioName]])
    scenarioResults <- ospsuite::runSimulations(
      simulations = simObj,
      population = sensitivityPopulation,
      simulationRunOptions = simulationRunOptions
    )
    if (length(scenarioResults) < 1) {
      stop("runSimulations returned no results")
    }
    pkanalyses <- ospsuite::calculatePKAnalyses(scenarioResults[[1]])
    ospsuite::exportPKAnalysesToCSV(
      pkAnalyses = pkanalyses,
      filePath = file.path(
        sensitivityFolder,
        paste0(scenarioName, "-PKAnalysisResults.csv")
      )
    )
  }

  sensitivityPopulationDT <- data.table::setDT(
    ospsuite::populationToDataFrame(sensitivityPopulation)
  )[, .(IndividualId, factor, SensitivityParameter)]

  pkVals <- loadSensitivityPKValues(
    scenarioFiles = scenarioFiles,
    outputPaths = outputPaths,
    pkParameter = pkParameter,
    outFolder = sensitivityFolder
  )

  pkParameterDTSens <- merge(
    sensitivityPopulationDT,
    data.table::setDT(pkVals),
    by = "IndividualId",
    sort = FALSE
  )
  pkParameterDTSens[, factor := as.numeric(factor)]

  baseline <- pkParameterDTSens[
    factor == 1,
    .(QuantityPath, PKParameter, Value.base = Value)
  ]
  pkParameterDTSens <- merge(
    pkParameterDTSens[factor != 1],
    baseline,
    by = c("QuantityPath", "PKParameter"),
    all.x = TRUE,
    sort = FALSE
  )

  if (any(is.na(pkParameterDTSens$Value.base))) {
    stop("Some perturbed rows have no matching baseline (factor == 1) row.")
  }

  pkParameterDTSens[, dPK := (Value - Value.base) / Value.base]
  pkParameterDTSens[, dP := factor - 1]
  pkParameterDTSens[, sens := dPK / dP]

  sens <- pkParameterDTSens[,
    .(sens = mean(sens, na.rm = TRUE)),
    by = .(SensitivityParameter, QuantityPath, PKParameter)
  ]

  data.table::fwrite(
    x = sens,
    file = file.path(
      sensitivityFolder,
      paste0(utils::tail(names(scenarioFiles), 1), "-sensitivity.csv")
    )
  )

  return(invisible(sens))
}

#' Load PK analysis results for sensitivity calculation
#'
#' Reads `<scenarioName>-PKAnalysisResults.csv` files written by
#' `ospsuite::exportPKAnalysesToCSV`. When two scenarios are supplied the
#' returned `Value` column contains `scenario2 / scenario1` (DDI ratio).
#'
#' @param scenarioFiles Named character vector (length 1 or 2) of scenario names.
#' @param outputPaths Character vector of `QuantityPath` values to keep.
#' @param pkParameter Character vector of parameter names to keep.
#' @param outFolder Folder containing the `<scenarioName>-PKAnalysisResults.csv` files.
#'
#' @return A data.table with columns `IndividualId`, `QuantityPath`, `PKParameter`, `Value`.
#' @export
#' @family sensitivity functions
loadSensitivityPKValues <- function(
  scenarioFiles,
  outputPaths,
  pkParameter,
  outFolder
) {
  checkmate::assertCharacter(scenarioFiles, min.len = 1, max.len = 2)
  checkmate::assertCharacter(outputPaths, any.missing = FALSE)
  checkmate::assertCharacter(pkParameter, any.missing = FALSE)
  checkmate::assertString(outFolder)

  refFile <- file.path(
    outFolder,
    paste0(names(scenarioFiles)[1], "-PKAnalysisResults.csv")
  )
  checkmate::assertFileExists(refFile, .var.name = "reference PK analysis file")
  dtPK <- data.table::fread(file = refFile)

  if (length(scenarioFiles) == 2) {
    numFile <- file.path(
      outFolder,
      paste0(names(scenarioFiles)[2], "-PKAnalysisResults.csv")
    )
    checkmate::assertFileExists(
      numFile,
      .var.name = "numerator PK analysis file"
    )
    dtNumerator <- data.table::fread(file = numFile)

    keyCols <- setdiff(
      intersect(names(dtPK), names(dtNumerator)),
      "Value"
    )
    if (length(keyCols) == 0) {
      stop("No common columns to merge the two PK analysis files.")
    }
    dtPK <- merge(
      dtPK,
      dtNumerator,
      by = keyCols,
      suffixes = c(".reference", ".numerator")
    )

    valueCols <- grep("^Value", names(dtPK), value = TRUE)
    if (!all(c("Value.reference", "Value.numerator") %in% valueCols)) {
      if (length(valueCols) >= 2) {
        data.table::setnames(
          dtPK,
          old = valueCols[1:2],
          new = c("Value.reference", "Value.numerator")
        )
      } else {
        stop(
          "Could not identify Value columns after merging PK analysis files."
        )
      }
    }
    dtPK[, Value := Value.numerator / Value.reference]
  }

  checkmate::assertNames(
    names(dtPK),
    must.include = c("QuantityPath", "Parameter")
  )
  dtPK <- dtPK[QuantityPath %in% outputPaths & Parameter %in% pkParameter]
  data.table::setnames(dtPK, old = "Parameter", new = "PKParameter")

  if (!("IndividualId" %in% names(dtPK))) {
    dtPK[, IndividualId := 0L]
  }

  keepCols <- intersect(
    c("IndividualId", "QuantityPath", "PKParameter", "Value"),
    names(dtPK)
  )
  return(dtPK[, ..keepCols])
}

#' Prepare a population for custom sensitivity analysis
#'
#' Builds a perturbed-parameter population suitable for passing to
#' `ospsuite::runSimulations`. For each entry in `sensitivityParameter`, all
#' listed model paths are multiplied by the same factor. The baseline row
#' (factor = 1) is always included.
#'
#' @param scenarioFiles Named character vector (length 1 or 2) of PKML paths.
#'   Baseline values are read from the first file; if a second is given, the
#'   baseline values must agree (checked with tolerance 1e-8).
#' @param sensitivityParameter Named list; element names are sensitivity labels,
#'   elements are character vectors of model parameter paths to vary together.
#' @param variationRange Numeric fraction variation range (default `0.1`).
#' @param numberOfSteps Integer number of positive perturbation steps (default `2`).
#'
#' @return An ospsuite population object.
#' @export
#' @family sensitivity functions
prepareSensitivityPopulation <- function(
  scenarioFiles,
  sensitivityParameter,
  variationRange = 0.1,
  numberOfSteps = 2
) {
  checkmate::assertCharacter(scenarioFiles, min.len = 1, max.len = 2)
  checkmate::assertList(sensitivityParameter, min.len = 1)
  checkmate::assertNumber(variationRange, lower = 0)
  checkmate::assertInt(numberOfSteps, lower = 1)

  positive <- 1 + variationRange * (seq_len(numberOfSteps) / numberOfSteps)
  factors <- sort(unique(c(positive, 1 / positive, 1)))

  sensitivityPaths <- unique(unlist(sensitivityParameter))

  sim1 <- ospsuite::loadSimulation(
    filePath = scenarioFiles[[1]],
    loadFromCache = FALSE
  )

  getParValue <- function(parPath, container) {
    res <- ospsuite::getParameter(
      path = parPath,
      container = container,
      stopIfNotFound = FALSE
    )
    if (is.null(res) || !("value" %in% names(res))) {
      return(NULL)
    }
    return(res)
  }

  baselineValues <- list()
  missingPaths <- character(0)
  for (parPath in sensitivityPaths) {
    res <- getParValue(parPath, sim1)
    if (is.null(res)) {
      warning(
        sprintf(
          "Parameter path not found in '%s': %s",
          scenarioFiles[[1]],
          parPath
        ),
        call. = FALSE
      )
      missingPaths <- c(missingPaths, parPath)
      baselineValues[[parPath]] <- NA_real_
    } else {
      baselineValues[[parPath]] <- as.numeric(res$value)
    }
  }

  if (length(scenarioFiles) == 2) {
    sim2 <- ospsuite::loadSimulation(
      filePath = scenarioFiles[[2]],
      loadFromCache = FALSE
    )
    for (parPath in sensitivityPaths) {
      res2 <- getParValue(parPath, sim2)
      if (is.null(res2)) {
        warning(
          sprintf(
            "Parameter path not found in '%s': %s",
            scenarioFiles[[2]],
            parPath
          ),
          call. = FALSE
        )
      } else {
        val2 <- as.numeric(res2$value)
        val1 <- baselineValues[[parPath]]
        if (is.na(val1)) {
          baselineValues[[parPath]] <- val2
          missingPaths <- setdiff(missingPaths, parPath)
        } else if (val1 == 0 && val2 != 0) {
          stop(sprintf(
            "Baseline mismatch for '%s': %g vs %g",
            parPath,
            val1,
            val2
          ))
        } else if (!isTRUE(all.equal(val2 / val1, 1, tolerance = 1e-8))) {
          stop(sprintf(
            "Baseline mismatch for '%s': %g vs %g",
            parPath,
            val1,
            val2
          ))
        }
      }
    }
  }

  availablePaths <- names(baselineValues)[!is.na(unlist(baselineValues))]
  if (length(availablePaths) == 0) {
    stop(
      "None of the sensitivity parameter paths were found in the scenario(s)."
    )
  }

  filteredSensitivityParameter <- list()
  for (sensName in names(sensitivityParameter)) {
    kept <- sensitivityParameter[[sensName]][
      sensitivityParameter[[sensName]] %in% availablePaths
    ]
    removed <- setdiff(sensitivityParameter[[sensName]], kept)
    if (length(removed) > 0) {
      warning(
        sprintf(
          "Paths not found for '%s' (skipped): %s",
          sensName,
          paste(removed, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (length(kept) > 0) {
      filteredSensitivityParameter[[sensName]] <- kept
    } else {
      warning(
        sprintf(
          "All paths missing for sensitivity parameter '%s'; skipped.",
          sensName
        ),
        call. = FALSE
      )
    }
  }
  if (length(filteredSensitivityParameter) == 0) {
    stop(
      "No valid sensitivity parameters remain after filtering missing paths."
    )
  }

  baselineValues <- baselineValues[availablePaths]
  populationDefault <- data.table::as.data.table(as.list(baselineValues))
  populationDefault[, factor := 1]
  populationDefault[, SensitivityParameter := ""]

  popPerParList <- lapply(
    names(filteredSensitivityParameter),
    function(sensName) {
      pathList <- filteredSensitivityParameter[[sensName]]
      data.table::rbindlist(
        lapply(factors[factors != 1], function(f) {
          row <- data.table::copy(populationDefault)
          for (parPath in pathList) {
            row[, (parPath) := as.numeric(get(parPath)) * f]
          }
          row[, factor := f]
          row[, SensitivityParameter := sensName]
          row
        }),
        use.names = TRUE,
        fill = TRUE
      )
    }
  )

  finalPopulation <- data.table::rbindlist(
    c(list(populationDefault), popPerParList),
    use.names = TRUE,
    fill = TRUE
  )
  finalPopulation[, IndividualId := .I - 1L]
  data.table::setcolorder(finalPopulation, "IndividualId")

  popFileName <- file.path(tempdir(), "Sensitivity-population.csv")
  data.table::fwrite(finalPopulation, file = popFileName)
  return(ospsuite::loadPopulation(popFileName))
}
