#' Add Sensitivity Table to Project Configuration
#'
#' This function adds a sensitivity table to the provided project configuration.
#' It loads a template Excel file and, when `scenarioList` is provided,
#' populates it with parameter paths derived across the supplied scenarios.
#'
#' @param projectConfiguration An object representing the project configuration.
#' @param scenarioList A named list of scenarios from which to extract
#'   parameter paths. If `NULL`, only the template workbook is added to the
#'   project configuration.
#' @param sheetName The name of the sheet in the Excel file to populate.
#' @param parameterPathMode Character string indicating how to combine the
#'   potential parameter paths from `scenarioList`: `"intersection"` keeps only
#'   paths present in every scenario, while `"union"` keeps any path present in
#'   at least one scenario.
#'
#' @return The modified project configuration object.
#' @export
addSensitivityTable <- function(
  projectConfiguration,
  scenarioList = NULL,
  sheetName = "Sensitivity",
  parameterPathMode = c("intersection", "union")
) {
  parameterPathMode <- match.arg(parameterPathMode)

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
    dt <- .sensitivityTableDataFromScenarios(
      scenarioList = scenarioList,
      sheetName = sheetName,
      parameterPathMode = parameterPathMode
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
#' This function executes sensitivity analysis for the specified scenarios in the
#' project configuration.
#' It checks for the presence of the sensitivity file, derives the PK
#' parameters and output paths configured for each scenario, and runs the
#' enhanced sensitivity workflow, saving the results to CSV files.
#'
#' Repeated rows in the sensitivity sheet that share the same `parameter`
#' value are treated as one grouped sensitivity label, so multiple
#' `parameterPath` values can be varied together.
#'
#' Ratio mode is only applied when `referenceScenario` is provided.
#' In that case the sensitivity is calculated for
#' `scenario / referenceScenario`.
#'
#' @param projectConfiguration An object representing the project configuration.
#' @param scenarioList A list of scenarios to analyze.
#' @param scenarioNames A vector of scenario names to run the analysis on. Defaults to NULL.
#' @param sensitivitysheet The name of the sheet in the sensitivity Excel file to use for analysis.
#' @param referenceScenario Optional scenario name used as the denominator for
#'   ratio mode. When provided, sensitivities are computed as
#'   \\code{scenario / referenceScenario}.
#' @param variationRange Numeric fraction defining how much each sensitivity
#'   parameter is varied around baseline (e.g., `0.1` means +/- 10\%).
#' @param numberOfSteps Integer number of perturbation steps on each side of
#'   baseline used for sensitivity estimation.
#' @param simulationRunOptions Optional `SimulationRunOptions` passed to the
#'   simulation runs.
#' @param overwrite A logical indicating whether to overwrite existing results. Defaults to TRUE.
#' @details Runs sensitivity analysis for each selected scenario and optionally
#'   computes scenario/reference ratios when `referenceScenario` is provided.
#'
#' @return NULL
#' @export
runSensitivityAnalysisForScenarios <-
  function(
    projectConfiguration,
    scenarioList,
    scenarioNames = NULL,
    sensitivitysheet,
    referenceScenario = NULL,
    variationRange = 0.1,
    numberOfSteps = 2,
    simulationRunOptions = ospsuite::SimulationRunOptions$new(
      showProgress = TRUE
    ),
    overwrite = TRUE
  ) {
    # initialize variable to avoid messages

    checkmate::assertList(
      scenarioList,
      min.len = 1,
      any.missing = FALSE,
      names = "named"
    )

    outputFolder <- .setupSensitivityRunner(projectConfiguration)
    runnerData <- .loadSensitivityRunnerData(
      projectConfiguration,
      sensitivitysheet
    )
    sensitivityParameter <- .asSensitivityParameterList(
      runnerData$sensitivityParameterDt
    )

    if (is.null(scenarioNames)) {
      scenarioNames <- names(scenarioList)
    }
    checkmate::assertString(referenceScenario, null.ok = TRUE)
    checkmate::assertCharacter(
      scenarioNames,
      min.len = 1,
      any.missing = FALSE
    )

    if (!is.null(referenceScenario) && referenceScenario %in% scenarioNames) {
      warning(
        messages$warningutilitiessensitivityReferenceScenarioExcluded(
          referenceScenario
        ),
        call. = FALSE
      )
      scenarioNames <- scenarioNames[scenarioNames != referenceScenario]
    }

    if (length(scenarioNames) == 0) {
      return(invisible(NULL))
    }

    scenarioType <- .sensitivityScenarioType(
      scenarioList = scenarioList,
      scenarioNames = scenarioNames
    )
    .assertSensitivityRunnerScenarioTypeCompatibility(
      runnerType = "standard",
      scenarioType = scenarioType
    )

    for (currentScenarioName in scenarioNames) {
      sensitivityResultFile <- file.path(
        outputFolder,
        .sensitivityAnalysisName(
          currentScenarioName,
          sensitivitysheet,
          referenceScenario = referenceScenario
        )
      )
      if (
        !file.exists(sensitivityResultFile) |
          overwrite
      ) {
        pkParameterSheets <- runnerData$dtScenarios[
          scenarioName == currentScenarioName & !is.na(pKParameter)
        ]$pKParameter
        if (length(pkParameterSheets) > 0) {
          .initializeParametersOfSheets(projectConfiguration, pkParameterSheets)

          scenarioFiles <- .getScenarioFilesForSensitivity(
            dtScenarios = runnerData$dtScenarios,
            currentScenarioName = currentScenarioName,
            modelFolder = projectConfiguration$modelFolder,
            referenceScenario = referenceScenario
          )

          pkDefinitions <- .getPKDefinitionsForSensitivity(
            projectConfiguration = projectConfiguration,
            pkParameterSheets = pkParameterSheets,
            dtOutputPaths = runnerData$dtOutputPaths
          )

          addMessageToLog(sprintf(
            "Calculating sensitivities for scenario '%s'.",
            currentScenarioName
          ))
          .calculateSensitivities(
            scenarioFiles = scenarioFiles,
            outputPaths = unique(pkDefinitions$outputPath),
            pkParameter = unique(pkDefinitions$pKParameterName),
            sensitivityParameter = sensitivityParameter,
            variationRange = variationRange,
            numberOfSteps = numberOfSteps,
            outFolder = projectConfiguration$outputFolder,
            resultFilePath = sensitivityResultFile,
            referenceScenario = referenceScenario,
            simulationRunOptions = simulationRunOptions
          )
        }
      }
    }
  }

#' Run representative-individual sensitivity analysis for population scenarios
#'
#' This function selects representative individuals from a scenario population
#' based on configured PK parameters and requested quantiles, then runs the
#' existing sensitivity workflow separately for each representative.
#'
#' Ratio mode is intentionally not supported for this workflow.
#'
#' @param projectConfiguration An object representing the project configuration.
#' @param scenarioList A named list of scenarios to analyze.
#' @param scenarioNames A character vector of scenario names to run. Defaults to
#'   `NULL`, which analyzes all scenarios in `scenarioList`.
#' @param sensitivitysheet The name of the sheet in the sensitivity Excel file
#'   to use for analysis.
#' @param representativeQuantiles Numeric vector of quantiles used to select
#'   representative individuals per PK parameter and output path. Defaults to
#'   `c(0.05, 0.5, 0.95)`.
#' @param variationRange Numeric fraction defining how much each sensitivity
#'   parameter is varied around baseline (e.g., `0.1` means +/- 10\%).
#' @param numberOfSteps Integer number of perturbation steps on each side of
#'   baseline used for sensitivity estimation.
#' @param simulationRunOptions Optional `SimulationRunOptions` passed to the
#'   simulation runs and prerequisite scenario simulations.
#' @param overwrite A logical indicating whether to overwrite existing results.
#'
#' @return Invisibly returns `NULL`.
#' @export
runRepresentativeSensitivityAnalysisForScenarios <- function(
  projectConfiguration,
  scenarioList,
  scenarioNames = NULL,
  sensitivitysheet,
  representativeQuantiles = c(0.05, 0.5, 0.95),
  variationRange = 0.1,
  numberOfSteps = 2,
  simulationRunOptions = ospsuite::SimulationRunOptions$new(
    showProgress = TRUE
  ),
  overwrite = TRUE
) {
  checkmate::assertList(scenarioList, any.missing = FALSE, names = "named")
  checkmate::assertNumeric(
    representativeQuantiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )

  outputFolder <- .setupSensitivityRunner(projectConfiguration)
  runnerData <- .loadSensitivityRunnerData(
    projectConfiguration,
    sensitivitysheet
  )
  sensitivityParameter <- .asSensitivityParameterList(
    runnerData$sensitivityParameterDt
  )

  if (is.null(scenarioNames)) {
    scenarioNames <- names(scenarioList)
  }

  scenarioType <- .sensitivityScenarioType(
    scenarioList = scenarioList,
    scenarioNames = scenarioNames
  )
  .assertSensitivityRunnerScenarioTypeCompatibility(
    runnerType = "representative",
    scenarioType = scenarioType
  )

  for (currentScenarioName in scenarioNames) {
    sensitivityResultFile <- file.path(
      outputFolder,
      .representativeSensitivityAnalysisName(
        currentScenarioName,
        sensitivitysheet
      )
    )

    if (!file.exists(sensitivityResultFile) | overwrite) {
      pkParameterSheets <- runnerData$dtScenarios[
        scenarioName == currentScenarioName & !is.na(pKParameter)
      ]$pKParameter
      if (length(pkParameterSheets) == 0) {
        next
      }

      .initializeParametersOfSheets(projectConfiguration, pkParameterSheets)
      .ensureRepresentativeSensitivityPrerequisites(
        projectConfiguration = projectConfiguration,
        scenarioList = scenarioList,
        scenarioName = currentScenarioName,
        simulationRunOptions = simulationRunOptions
      )

      scenarioFiles <- .getScenarioFilesForSensitivity(
        dtScenarios = runnerData$dtScenarios,
        currentScenarioName = currentScenarioName,
        modelFolder = projectConfiguration$modelFolder,
        referenceScenario = NULL
      )

      pkDefinitions <- .getPKDefinitionsForSensitivity(
        projectConfiguration = projectConfiguration,
        pkParameterSheets = pkParameterSheets,
        dtOutputPaths = runnerData$dtOutputPaths
      )

      representatives <- .getRepresentativeIndividualsForSensitivity(
        outputFolder = projectConfiguration$outputFolder,
        scenarioName = currentScenarioName,
        outputPaths = unique(pkDefinitions$outputPath),
        pkParameter = unique(pkDefinitions$pKParameterName),
        representativeQuantiles = representativeQuantiles
      )
      representatives <- representatives[!is.na(RepresentativeIndividualId)]

      if (nrow(representatives) == 0) {
        warning(
          sprintf(
            "No representative individuals could be selected for scenario '%s'.",
            currentScenarioName
          ),
          call. = FALSE
        )
        next
      }
      scenarioPopulation <- scenarioList[[currentScenarioName]]$population

      representativeResults <- lapply(
        split(representatives, seq_len(nrow(representatives))),
        function(representative) {
          representativePopulation <-
            .prepareRepresentativeSensitivityPopulation(
              population = scenarioPopulation,
              representativeIndividualId = representative$RepresentativeIndividualId,
              sensitivityParameter = sensitivityParameter,
              scenarioSimulation = scenarioList[[
                currentScenarioName
              ]]$simulation,
              variationRange = variationRange,
              numberOfSteps = numberOfSteps
            )
          addMessageToLog(sprintf(
            paste0(
              "Calculating sensitivities for scenario '%s' ",
              "(representative: %s, PK parameter: %s, output path: %s)."
            ),
            currentScenarioName,
            representative$RepresentativeQuantile,
            representative$RepresentativePKParameter,
            representative$RepresentativeOutputPath
          ))

          sens <- .calculateSensitivities(
            scenarioFiles = scenarioFiles,
            outputPaths = as.character(representative$RepresentativeOutputPath),
            pkParameter = as.character(
              representative$RepresentativePKParameter
            ),
            sensitivityParameter = sensitivityParameter,
            variationRange = variationRange,
            numberOfSteps = numberOfSteps,
            outFolder = projectConfiguration$outputFolder,
            simulationRunOptions = simulationRunOptions,
            writeResult = FALSE,
            population = representativePopulation
          )

          sens[,
            OutputPath := as.character(
              representative$RepresentativeOutputPath
            )
          ]
          sens[,
            PKParameter := as.character(
              representative$RepresentativePKParameter
            )
          ]
          sens[,
            RepresentativeQuantile := as.character(
              representative$RepresentativeQuantile
            )
          ]
          sens[,
            RepresentativeIndividualId := representative$RepresentativeIndividualId
          ]
          sens
        }
      )

      representativeResults <- data.table::rbindlist(
        representativeResults,
        use.names = TRUE,
        fill = TRUE
      )
      data.table::fwrite(representativeResults, file = sensitivityResultFile)
    }
  }

  return(invisible(NULL))
}


#' Build sensitivity table data from scenarios.
#'
#' Internal helper that extracts potential variable parameter paths from each
#' scenario simulation and combines them according to `parameterPathMode`.
#'
#' @param scenarioList Named list of scenarios. Each element must provide a
#'   `simulation` object.
#' @param sheetName Name of the target Excel sheet. Used for validation
#'   consistency with the caller.
#' @param parameterPathMode Character string indicating how to combine
#'   parameter paths: `"intersection"` or `"union"`.
#'
#' @return A data.table with columns `parameter` and `parameterPath`.
#' @keywords internal
#' @noRd
.sensitivityTableDataFromScenarios <- function(
  scenarioList,
  sheetName,
  parameterPathMode
) {
  checkmate::assertList(scenarioList, min.len = 1, names = "named")
  checkmate::assertString(sheetName, max.chars = 31)
  parameterPathMode <- match.arg(parameterPathMode, c("intersection", "union"))

  parameterPathsPerScenario <- lapply(scenarioList, function(oneScenario) {
    ospsuite::potentialVariableParameterPathsFor(oneScenario$simulation)
  })
  parameterPaths <- switch(
    parameterPathMode,
    intersection = Reduce(intersect, parameterPathsPerScenario),
    union = Reduce(union, parameterPathsPerScenario)
  )
  parameterPaths <- unique(parameterPaths)

  if (length(parameterPaths) == 0) {
    stop(messages$errorutilitiessensitivityNoParameterPaths(parameterPathMode))
  }

  sensitivityTableData <- data.table(
    parameter = gsub("\\|", " ", parameterPaths),
    parameterPath = parameterPaths
  )

  return(sensitivityTableData)
}

#' Classify selected scenarios by population availability.
#'
#' @param scenarioList Named scenario list.
#' @param scenarioNames Optional subset of scenario names.
#'
#' @return One of `"population"`, `"individual"`, or `"mixed"`.
#' @keywords internal
#' @noRd
.sensitivityScenarioType <- function(scenarioList, scenarioNames = NULL) {
  checkmate::assertList(scenarioList, min.len = 1, names = "named")
  namesToCheck <- scenarioNames %||% names(scenarioList)
  checkmate::assertCharacter(namesToCheck, min.len = 1, any.missing = FALSE)
  checkmate::assertSubset(namesToCheck, names(scenarioList))

  hasPopulation <- vapply(
    namesToCheck,
    function(oneScenarioName) {
      oneScenario <- scenarioList[[oneScenarioName]]
      !is.null(oneScenario$population) &&
        inherits(oneScenario$population, "Population")
    },
    logical(1)
  )

  if (all(hasPopulation)) {
    return("population")
  }
  if (all(!hasPopulation)) {
    return("individual")
  }
  return("mixed")
}

#' Assert runner choice matches selected scenario type.
#'
#' @param runnerType Runner kind: `"standard"` or `"representative"`.
#' @param scenarioType Scenario type from `.sensitivityScenarioType`.
#' @return NULL (invisible).
#'
#' @keywords internal
#' @noRd
.assertSensitivityRunnerScenarioTypeCompatibility <- function(
  runnerType,
  scenarioType
) {
  checkmate::assertChoice(runnerType, c("standard", "representative"))
  checkmate::assertChoice(scenarioType, c("population", "individual", "mixed"))

  if (runnerType == "standard" && scenarioType == "population") {
    stop(
      messages$errorutilitiessensitivityPopulationScenariosForStandardRunner()
    )
  }

  if (runnerType == "representative" && scenarioType == "individual") {
    stop(
      messages$errorutilitiessensitivityIndividualScenariosForRepresentativeRunner()
    )
  }

  return(invisible(NULL))
}

#' Validate sensitivity inputs and ensure output folder exists.
#'
#' @param projectConfiguration A project configuration object.
#' @return Character path to the sensitivity output folder.
#' @keywords internal
#' @noRd
.setupSensitivityRunner <- function(projectConfiguration) {
  if (!("sensitivityFile" %in% names(projectConfiguration$addOns))) {
    stop(messages$errorutilitiessensitivityMissingSensitivityParameterConfig())
  }

  outputFolder <- file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$sensitivityResults
  )
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  return(outputFolder)
}

#' Load shared tables for sensitivity runners.
#'
#' @param projectConfiguration A project configuration object.
#' @param sensitivitysheet Character sheet name to read from the sensitivity workbook.
#' @return Named list with scenario, output-path and sensitivity-parameter tables.
#' @keywords internal
#' @noRd
.loadSensitivityRunnerData <- function(projectConfiguration, sensitivitysheet) {
  dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)
  dtOutputPaths <- getOutputPathIds(projectConfiguration$addOns$reportsFile)

  sensitivityParameterDt <- xlsxReadData(
    projectConfiguration$addOns$sensitivityFile,
    sheetName = sensitivitysheet,
    skipDescriptionRow = TRUE
  )

  return(list(
    dtScenarios = dtScenarios,
    dtOutputPaths = dtOutputPaths,
    sensitivityParameterDt = sensitivityParameterDt
  ))
}


#' Generate Sensitivity Analysis Output File Name
#'
#' This function generates a name for the sensitivity analysis output file based on the scenario name and sheet name.
#'
#' @param scenarioName The name of the scenario.
#' @param sensitivitysheet The name of the sensitivity sheet.
#' @param referenceScenario Optional reference scenario name used in ratio mode.
#'
#' @return A string representing the generated file name for the sensitivity analysis results.
#' @keywords internal
#' @noRd
.sensitivityAnalysisName <- function(
  scenarioName,
  sensitivitysheet,
  referenceScenario = NULL
) {
  fileStem <- paste0(scenarioName, "_", sensitivitysheet)

  if (!is.null(referenceScenario)) {
    referenceScenario <- as.character(referenceScenario)
    if (length(referenceScenario) == 1 && length(fileStem) > 1) {
      referenceScenario <- rep(referenceScenario, length(fileStem))
    }

    hasReferenceScenario <- !is.na(referenceScenario) &
      nzchar(referenceScenario)
    fileStem[hasReferenceScenario] <- paste0(
      fileStem[hasReferenceScenario],
      "_",
      referenceScenario[hasReferenceScenario]
    )
  }

  return(paste0(fileStem, ".csv"))
}

#' Build representative sensitivity result file name.
#'
#' @param scenarioName Character scenario name.
#' @param sensitivitysheet Character sensitivity sheet name.
#' @return Character file name for representative sensitivity results.
#' @keywords internal
#' @noRd
.representativeSensitivityAnalysisName <- function(
  scenarioName,
  sensitivitysheet
) {
  return(paste0(scenarioName, "_", sensitivitysheet, "_representatives.csv"))
}

#' Resolve scenario PKML files for sensitivity analysis.
#'
#' @param dtScenarios Data table with scenario definitions.
#' @param currentScenarioName Character scenario name to evaluate.
#' @param modelFolder Character folder containing PKML files.
#' @param referenceScenario Optional reference scenario name.
#' @return Named character vector of PKML file paths.
#' @keywords internal
#' @noRd
.getScenarioFilesForSensitivity <- function(
  dtScenarios,
  currentScenarioName,
  modelFolder,
  referenceScenario = NULL
) {
  scenarioNames <- currentScenarioName
  if (!is.null(referenceScenario)) {
    if (identical(referenceScenario, currentScenarioName)) {
      stop(messages$errorutilitiessensitivityReferenceScenarioMustDiffer())
    }
    scenarioNames <- c(referenceScenario, currentScenarioName)
  }

  modelFiles <- lapply(scenarioNames, function(oneScenarioName) {
    oneModelFile <- unique(dtScenarios[
      scenarioName == oneScenarioName,
      modelFile
    ])
    oneModelFile <- oneModelFile[!is.na(oneModelFile) & nzchar(oneModelFile)]
    if (length(oneModelFile) != 1) {
      stop(sprintf(
        "Scenario '%s' must resolve to exactly one modelFile entry.",
        oneScenarioName
      ))
    }
    oneModelFile
  })

  return(stats::setNames(
    file.path(modelFolder, unlist(modelFiles, use.names = FALSE)),
    scenarioNames
  ))
}

#' Read PK definition rows from configured PK parameter sheets.
#'
#' @param projectConfiguration A project configuration object.
#' @param pkParameterSheets Character vector of PK parameter sheet names.
#' @return Data table with normalized PK definitions from all requested sheets.
#' @keywords internal
#' @noRd
.readPKDefinitionsFromSheets <- function(
  projectConfiguration,
  pkParameterSheets
) {
  pkParameterSheets <- unique(splitInputs(pkParameterSheets))
  pkParameterSheets <- pkParameterSheets[
    !is.na(pkParameterSheets) & nzchar(pkParameterSheets)
  ]
  checkmate::assertCharacter(
    pkParameterSheets,
    any.missing = FALSE,
    min.len = 1
  )

  return(
    lapply(splitInputs(pkParameterSheets), function(sheetName) {
      xlsxReadData(
        wb = projectConfiguration$addOns$pKParameterFile,
        sheetName = sheetName,
        skipDescriptionRow = TRUE
      )
    }) %>%
      data.table::rbindlist(fill = TRUE, use.names = TRUE) %>%
      .separateAndTrimColumn(columnName = "outputPathIds")
  )
}

#' Normalize PK definition column names for sensitivity workflows.
#'
#' @param pkDefinitions Data table with PK definition columns.
#' @return Data table with normalized PK definition column names.
#' @keywords internal
#' @noRd
.normalizedPKDefinitionsForSensitivity <- function(pkDefinitions) {
  if ("descriptions" %in% names(pkDefinitions)) {
    pkDefinitions[, descriptions := NULL]
  }

  return(
    pkDefinitions %>%
      setnames(
        old = c("name", "displayName", "displayUnit"),
        new = c(
          "pKParameter",
          "displayNamePKParameter",
          "displayUnitPKParameter"
        )
      )
  )
}

#' Build PK definition table for sensitivity analysis.
#'
#' @param projectConfiguration A project configuration object.
#' @param pkParameterSheets Character vector of PK parameter sheet names.
#' @param dtOutputPaths Data table containing output path IDs and paths.
#' @return Data table with PK parameter names mapped to output paths.
#' @keywords internal
#' @noRd
.getPKDefinitionsForSensitivity <- function(
  projectConfiguration,
  pkParameterSheets,
  dtOutputPaths
) {
  pkDefinitions <- .readPKDefinitionsFromSheets(
    projectConfiguration = projectConfiguration,
    pkParameterSheets = pkParameterSheets
  ) %>%
    .normalizedPKDefinitionsForSensitivity() %>%
    setnames(old = "pKParameter", new = "pKParameterName") %>%
    merge(
      dtOutputPaths[, c("outputPathId", "outputPath")],
      by = "outputPathId",
      all.x = TRUE,
      sort = FALSE
    )

  checkmate::assertNames(
    names(pkDefinitions),
    must.include = c("pKParameterName", "outputPath")
  )

  pkDefinitions <- pkDefinitions[
    !is.na(outputPath) & !is.na(pKParameterName)
  ]

  if (nrow(pkDefinitions) == 0) {
    stop(messages$errorutilitiessensitivityNoPKDefinitionsForSensitivity())
  }

  return(pkDefinitions)
}

#' Normalize sensitivity parameter table columns and values.
#'
#' @param sensitivityParameterDt Data frame with configured sensitivity parameters.
#' @return Data table containing unique parameter names and parameter paths.
#' @keywords internal
#' @noRd
.normalizedSensitivityParameterTable <- function(sensitivityParameterDt) {
  checkmate::assertDataFrame(sensitivityParameterDt, min.rows = 1)
  checkmate::assertNames(
    names(sensitivityParameterDt),
    must.include = "parameterPath"
  )

  normalizedTable <- data.table::copy(data.table::as.data.table(
    sensitivityParameterDt
  ))
  if (!"parameter" %in% names(normalizedTable)) {
    normalizedTable[, "parameter" := normalizedTable[["parameterPath"]]]
  }

  normalizedTable[["parameterPath"]] <- as.character(normalizedTable[[
    "parameterPath"
  ]])
  normalizedTable[["parameter"]] <- as.character(normalizedTable[["parameter"]])
  normalizedTable <- normalizedTable[
    !is.na(normalizedTable[["parameterPath"]]) &
      nzchar(normalizedTable[["parameterPath"]])
  ]

  if (nrow(normalizedTable) == 0) {
    stop(messages$errorutilitiessensitivityMissingParameterPathValues())
  }

  missingParameter <- is.na(normalizedTable[["parameter"]]) |
    !nzchar(normalizedTable[["parameter"]])
  normalizedTable[["parameter"]][missingParameter] <-
    normalizedTable[["parameterPath"]][missingParameter]

  normalizedTable[["parameterName"]] <- normalizedTable[["parameter"]]

  return(unique(normalizedTable[, c("parameterName", "parameterPath")]))
}

#' Convert normalized sensitivity parameters to named list.
#'
#' @param sensitivityParameterDt Data frame with configured sensitivity parameters.
#' @return Named list of parameter paths grouped by sensitivity parameter name.
#' @keywords internal
#' @noRd
.asSensitivityParameterList <- function(sensitivityParameterDt) {
  normalizedTable <- .normalizedSensitivityParameterTable(
    sensitivityParameterDt
  )
  sensitivityParameter <- split(
    normalizedTable$parameterPath,
    normalizedTable$parameterName
  )

  return(lapply(sensitivityParameter, unique))
}

#' Internal helper for scenario sensitivity calculation.
#'
#' @param scenarioFiles Named character vector (length 1 or 2) of PKML file paths.
#'   Names become output file prefixes.
#' @param outputPaths Character vector of \code{QuantityPath} values to keep.
#' @param pkParameter Character vector of PK parameter names to use.
#' @param sensitivityParameter Named list; each element name is a sensitivity
#'   label and each element is a character vector of model parameter paths
#'   to vary for that label.
#' @param variationRange Numeric fraction variation range (default \code{0.1} = +/- 10\%).
#' @param numberOfSteps Integer number of positive perturbation steps (default \code{2}).
#' @param outFolder Base output folder; a \code{SensitivityResults/} subfolder is created.
#' @param simulationRunOptions Optional \code{SimulationRunOptions} passed to ospsuite.
#' @param resultFilePath Optional explicit file path for the final sensitivity
#'   CSV. If omitted, the file is written as
#'   \\code{<lastScenarioName>-sensitivity.csv} under
#'   \\code{<outFolder>/SensitivityResults/}.
#' @param referenceScenario Optional reference scenario name. When provided,
#'   a `referenceScenario` column with this value is added to the result.
#' @param writeResult Logical flag indicating whether the result CSV should be written.
#' @param population Optional ospsuite `Population` object to use instead of building one.
#' @details This helper runs simulations, computes PK analyses, and derives
#'   sensitivity coefficients as relative PK change over relative parameter change.
#'
#' @return Invisibly returns a data.table with columns
#'   \code{SensitivityParameter}, \code{QuantityPath}, \code{PKParameter},
#'   \code{sens}, and \code{referenceScenario}.
#' @keywords internal
#' @noRd
.calculateSensitivities <- function(
  scenarioFiles,
  outputPaths,
  pkParameter,
  sensitivityParameter,
  variationRange = 0.1,
  numberOfSteps = 2,
  outFolder = tempdir(),
  simulationRunOptions = NULL,
  resultFilePath = NULL,
  referenceScenario = NULL,
  writeResult = TRUE,
  population = NULL
) {
  # avoid warnings for global variables during check

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
  checkmate::assertString(resultFilePath, null.ok = TRUE)
  checkmate::assertString(referenceScenario, null.ok = TRUE)
  checkmate::assertFlag(writeResult)
  for (fp in scenarioFiles) {
    checkmate::assertFileExists(fp, .var.name = paste("scenarioFile", fp))
  }
  if (!is.null(population)) {
    checkmate::assertClass(population, classes = "Population")
  }
  pkAnalysisFolder <- tempfile(pattern = "sensitivity-pk-")
  dir.create(pkAnalysisFolder)
  on.exit(unlink(pkAnalysisFolder, recursive = TRUE, force = TRUE), add = TRUE)

  sensitivityFolder <- file.path(outFolder, EXPORTDIR$sensitivityResults)
  if (!dir.exists(sensitivityFolder)) {
    dir.create(sensitivityFolder, recursive = TRUE)
  }

  sensitivityPopulation <- population %||%
    .prepareSensitivityPopulation(
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
        pkAnalysisFolder,
        paste0(scenarioName, "-PKAnalysisResults.csv")
      )
    )
  }

  sensitivityPopulationDT <- data.table::setDT(
    ospsuite::populationToDataFrame(sensitivityPopulation)
  )
  if (
    !"perturbationFactor" %in% names(sensitivityPopulationDT) &&
      "factor" %in% names(sensitivityPopulationDT)
  ) {
    data.table::setnames(
      sensitivityPopulationDT,
      old = "factor",
      new = "perturbationFactor"
    )
  }
  sensitivityPopulationDT <- sensitivityPopulationDT[,
    .(IndividualId, perturbationFactor, SensitivityParameter)
  ]

  pkVals <- .loadSensitivityPKValues(
    scenarioFiles = scenarioFiles,
    outputPaths = outputPaths,
    pkParameter = pkParameter,
    outFolder = pkAnalysisFolder
  )

  pkParameterDTSens <- merge(
    sensitivityPopulationDT,
    data.table::setDT(pkVals),
    by = "IndividualId",
    sort = FALSE
  )
  pkParameterDTSens[, perturbationFactor := as.numeric(perturbationFactor)]

  baseline <- pkParameterDTSens[
    perturbationFactor == 1,
    .(QuantityPath, PKParameter, Value.base = Value)
  ]
  pkParameterDTSens <- merge(
    pkParameterDTSens[perturbationFactor != 1],
    baseline,
    by = c("QuantityPath", "PKParameter"),
    all.x = TRUE,
    sort = FALSE
  )

  if (any(is.na(pkParameterDTSens$Value.base))) {
    stop(
      "Some perturbed rows have no matching baseline (perturbationFactor == 1) row."
    )
  }

  pkParameterDTSens[, dPK := (Value - Value.base) / Value.base]
  pkParameterDTSens[, dP := perturbationFactor - 1]
  pkParameterDTSens[, sens := dPK / dP]

  sens <- pkParameterDTSens[,
    .(sens = mean(sens, na.rm = TRUE)),
    by = .(SensitivityParameter, QuantityPath, PKParameter)
  ]
  sens[, referenceScenario := referenceScenario %||% NA_character_]

  if (writeResult) {
    if (is.null(resultFilePath)) {
      resultFilePath <- file.path(
        sensitivityFolder,
        paste0(utils::tail(names(scenarioFiles), 1), "-sensitivity.csv")
      )
    }

    data.table::fwrite(x = sens, file = resultFilePath)
  }

  return(invisible(sens))
}

#' Format quantile labels for representative sensitivity outputs.
#'
#' @param representativeQuantiles Numeric vector of quantiles in [0, 1].
#' @return Character vector of formatted quantile labels.
#' @keywords internal
#' @noRd
.representativeQuantileLabels <- function(representativeQuantiles) {
  return(formatPercentiles(
    percentiles = representativeQuantiles,
    suffix = " percentile",
    allAsPercentiles = FALSE
  ))
}

#' Select representative individuals from PK analysis outputs.
#'
#' @param outputFolder Character path containing PK analysis outputs.
#' @param scenarioName Character scenario name.
#' @param outputPaths Character vector of output paths.
#' @param pkParameter Character vector of PK parameter names.
#' @param representativeQuantiles Numeric quantiles used to pick representative individuals.
#' @return Data table mapping representative quantiles to individual IDs by output/parameter.
#' @keywords internal
#' @noRd
.getRepresentativeIndividualsForSensitivity <- function(
  outputFolder,
  scenarioName,
  outputPaths,
  pkParameter,
  representativeQuantiles = c(0.05, 0.5, 0.95)
) {
  checkmate::assertString(outputFolder)
  checkmate::assertString(scenarioName)
  checkmate::assertCharacter(outputPaths, any.missing = FALSE, min.len = 1)
  checkmate::assertCharacter(pkParameter, any.missing = FALSE, min.len = 1)
  checkmate::assertNumeric(
    representativeQuantiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )

  pkFilePath <- file.path(
    outputFolder,
    EXPORTDIR$pKAnalysisResults,
    paste0(scenarioName, ".csv")
  )
  checkmate::assertFileExists(pkFilePath)

  pkResults <- data.table::fread(pkFilePath)
  checkmate::assertNames(
    names(pkResults),
    must.include = c("QuantityPath", "Parameter", "Value", "IndividualId")
  )

  pkResults <- pkResults[
    QuantityPath %in% outputPaths & Parameter %in% pkParameter
  ]
  if (nrow(pkResults) == 0) {
    stop(sprintf(
      "No PK analysis rows available for scenario '%s' and the configured output paths/PK parameters.",
      scenarioName
    ))
  }

  quantileLabels <- .representativeQuantileLabels(representativeQuantiles)

  nearestRepresentative <- function(value, individualId) {
    nonMissing <- !is.na(value)
    values <- value[nonMissing]
    individualIds <- individualId[nonMissing]

    if (length(values) == 0) {
      return(as.list(stats::setNames(
        rep(NA_character_, length(representativeQuantiles)),
        quantileLabels
      )))
    }

    quantiles <- stats::quantile(
      values,
      probs = representativeQuantiles,
      names = FALSE
    )

    closestIndex <- vapply(
      quantiles,
      function(q) {
        which.min(abs(values - q))
      },
      integer(1)
    )

    as.list(stats::setNames(individualIds[closestIndex], quantileLabels))
  }

  representatives <- pkResults[,
    nearestRepresentative(Value, IndividualId),
    by = .(QuantityPath, Parameter)
  ] |>
    melt(
      measure.vars = quantileLabels,
      variable.name = "RepresentativeQuantile",
      value.name = "RepresentativeIndividualId"
    ) |>
    setnames(
      old = c("QuantityPath", "Parameter"),
      new = c("RepresentativeOutputPath", "RepresentativePKParameter")
    )

  representatives[["RepresentativeQuantile"]] <- as.character(
    representatives[["RepresentativeQuantile"]]
  )

  return(representatives)
}

#' Ensure simulation and PK outputs exist for representative workflow.
#'
#' @param projectConfiguration A project configuration object.
#' @param scenarioList Named list of scenarios.
#' @param scenarioName Character scenario name.
#' @param simulationRunOptions Optional simulation run options.
#' @return Invisibly returns `NULL` after ensuring prerequisite files exist.
#' @keywords internal
#' @noRd
.ensureRepresentativeSensitivityPrerequisites <- function(
  projectConfiguration,
  scenarioList,
  scenarioName,
  simulationRunOptions = NULL
) {
  simulationOutputFolder <- file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$simulationResult
  )
  pkOutputFile <- file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$pKAnalysisResults,
    paste0(scenarioName, ".csv")
  )
  populationOutputFile <- file.path(
    simulationOutputFolder,
    paste0(scenarioName, "_population.csv")
  )

  if (file.exists(pkOutputFile) && file.exists(populationOutputFile)) {
    return(invisible(NULL))
  }

  runAndSaveScenarios(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList[scenarioName],
    simulationRunOptions = simulationRunOptions
  )

  checkmate::assertFileExists(pkOutputFile)
  checkmate::assertFileExists(populationOutputFile)

  return(invisible(NULL))
}


#' Keep only sensitivity parameter paths available in baseline data.
#'
#' @param sensitivityParameter Named list of sensitivity parameter paths.
#' @param availablePaths Character vector of available baseline paths.
#' @return Filtered named list containing only available paths.
#' @keywords internal
#' @noRd
.filterSensitivityParametersForAvailablePaths <- function(
  sensitivityParameter,
  availablePaths
) {
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

  return(filteredSensitivityParameter)
}

#' Build perturbed sensitivity population from one baseline row.
#'
#' @param baselineRow Single-row data frame with baseline values.
#' @param sensitivityParameter Named list of sensitivity parameter paths.
#' @param simulation Optional simulation used to resolve missing parameter paths.
#' @param variationRange Numeric fraction variation range.
#' @param numberOfSteps Integer number of perturbation steps.
#' @return An ospsuite `Population` object with perturbed individuals.
#' @keywords internal
#' @noRd
.buildSensitivityPopulationFromBaselineRow <- function(
  baselineRow,
  sensitivityParameter,
  simulation = NULL,
  variationRange = 0.1,
  numberOfSteps = 2
) {
  checkmate::assertDataFrame(baselineRow, nrows = 1)
  checkmate::assertList(sensitivityParameter, min.len = 1)
  checkmate::assertNumber(variationRange, lower = 0)
  checkmate::assertInt(numberOfSteps, lower = 1)

  baselineRow <- data.table::as.data.table(baselineRow)
  baselineRow <- .addSimulationParametersToBaselineRow(
    baselineRow = baselineRow,
    sensitivityParameter = sensitivityParameter,
    simulation = simulation
  )

  positive <- 1 + variationRange * (seq_len(numberOfSteps) / numberOfSteps)
  factors <- sort(unique(c(positive, 1 / positive, 1)))

  availablePaths <- setdiff(names(baselineRow), "IndividualId")
  filteredSensitivityParameter <- .filterSensitivityParametersForAvailablePaths(
    sensitivityParameter = sensitivityParameter,
    availablePaths = availablePaths
  )

  populationDefault <- data.table::copy(data.table::as.data.table(
    baselineRow[, availablePaths, with = FALSE]
  ))
  populationDefault[, perturbationFactor := 1]
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
          row[, perturbationFactor := f]
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

  popFileName <- tempfile(pattern = "Sensitivity-population-", fileext = ".csv")
  data.table::fwrite(finalPopulation, file = popFileName)
  return(ospsuite::loadPopulation(popFileName))
}

#' Add simulation-only sensitivity parameters to baseline row.
#'
#' @param baselineRow Single-row data table with baseline values.
#' @param sensitivityParameter Named list of requested sensitivity parameter paths.
#' @param simulation Optional simulation used to resolve missing paths.
#' @details If requested paths are absent from `baselineRow`, parameter values
#'   are looked up in `simulation` and appended when found.
#' @return Baseline row enriched with additional parameter columns when available.
#' @keywords internal
#' @noRd
.addSimulationParametersToBaselineRow <- function(
  baselineRow,
  sensitivityParameter,
  simulation = NULL
) {
  if (is.null(simulation)) {
    return(baselineRow)
  }

  requestedPaths <- unique(unlist(sensitivityParameter, use.names = FALSE))
  if (length(requestedPaths) == 0) {
    return(baselineRow)
  }

  missingPaths <- setdiff(requestedPaths, names(baselineRow))
  if (length(missingPaths) == 0) {
    return(baselineRow)
  }

  for (parPath in missingPaths) {
    res <- ospsuite::getParameter(
      path = parPath,
      container = simulation,
      stopIfNotFound = FALSE
    )
    if (is.null(res) || !("value" %in% names(res))) {
      next
    }
    baselineRow[, (parPath) := as.numeric(res$value)]
  }

  return(baselineRow)
}

#' Prepare a sensitivity population for one representative individual
#'
#' @param population An ospsuite `Population` object.
#' @param representativeIndividualId Individual ID to extract from the source
#'   population.
#' @param sensitivityParameter Named list of sensitivity parameter paths.
#' @param scenarioSimulation Optional ospsuite simulation used to resolve
#'   sensitivity parameter paths that are not present as population columns.
#' @param variationRange Numeric fraction variation range.
#' @param numberOfSteps Integer number of perturbation steps.
#'
#' @return An ospsuite population object.
#' @keywords internal
#' @noRd
.prepareRepresentativeSensitivityPopulation <- function(
  population,
  representativeIndividualId,
  sensitivityParameter,
  scenarioSimulation = NULL,
  variationRange = 0.1,
  numberOfSteps = 2
) {
  populationDt <- data.table::as.data.table(
    ospsuite::populationToDataFrame(population)
  )
  checkmate::assertNames(
    names(populationDt),
    must.include = "IndividualId"
  )
  baselineRow <- populationDt[
    IndividualId == representativeIndividualId,
    ,
    drop = FALSE
  ]
  if (nrow(baselineRow) != 1) {
    stop(sprintf(
      "Representative IndividualId '%s' was not found exactly once in the scenario population.",
      representativeIndividualId
    ))
  }

  return(.buildSensitivityPopulationFromBaselineRow(
    baselineRow = baselineRow,
    sensitivityParameter = sensitivityParameter,
    simulation = scenarioSimulation,
    variationRange = variationRange,
    numberOfSteps = numberOfSteps
  ))
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
#' @keywords internal
#' @noRd
.loadSensitivityPKValues <- function(
  scenarioFiles,
  outputPaths,
  pkParameter,
  outFolder
) {
  # avoid warnings for global variables during check

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
      stop(messages$errorutilitiessensitivityNoCommonColumnsToMergePKAnalysis())
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
  return(dtPK[, keepCols, with = FALSE])
}

#' Retrieve one parameter value from an ospsuite simulation
#'
#' @param parPath Character model parameter path.
#' @param simulation ospsuite simulation container.
#' @return Numeric value if found, otherwise `NULL`.
#' @keywords internal
#' @noRd
.getSensitivityParameterValue <- function(parPath, simulation) {
  parameterResult <- ospsuite::getParameter(
    path = parPath,
    container = simulation,
    stopIfNotFound = FALSE
  )
  if (is.null(parameterResult) || !("value" %in% names(parameterResult))) {
    return(NULL)
  }

  return(as.numeric(parameterResult$value))
}

#' Read baseline parameter values from one scenario simulation
#'
#' @param sensitivityPaths Character vector of sensitivity parameter paths.
#' @param simulation ospsuite simulation container.
#' @param scenarioFile Character scenario file path used for warning text.
#' @return List with named numeric list `baselineValues` and `missingPaths` vector.
#' @keywords internal
#' @noRd
.readSensitivityBaselineValues <- function(
  sensitivityPaths,
  simulation,
  scenarioFile
) {
  baselineValues <- list()
  missingPaths <- character(0)

  for (parPath in sensitivityPaths) {
    parameterValue <- .getSensitivityParameterValue(parPath, simulation)
    if (is.null(parameterValue)) {
      warning(
        sprintf(
          "Parameter path not found in '%s': %s",
          scenarioFile,
          parPath
        ),
        call. = FALSE
      )
      missingPaths <- c(missingPaths, parPath)
      baselineValues[[parPath]] <- NA_real_
    } else {
      baselineValues[[parPath]] <- parameterValue
    }
  }

  return(list(
    baselineValues = baselineValues,
    missingPaths = missingPaths
  ))
}

#' Assert that two baseline values are compatible
#'
#' @param parPath Character model parameter path.
#' @param valueScenario1 Numeric baseline value from first scenario.
#' @param valueScenario2 Numeric baseline value from second scenario.
#' @return NULL (invisible) when values are compatible.
#' @keywords internal
#' @noRd
.assertSensitivityBaselineCompatibility <- function(
  parPath,
  valueScenario1,
  valueScenario2
) {
  isIncompatible <-
    (valueScenario1 == 0 && valueScenario2 != 0) ||
    !isTRUE(all.equal(valueScenario2 / valueScenario1, 1, tolerance = 1e-8))

  if (isIncompatible) {
    stop(sprintf(
      "Baseline mismatch for '%s': %g vs %g",
      parPath,
      valueScenario1,
      valueScenario2
    ))
  }

  return(invisible(NULL))
}

#' Reconcile one parameter path against second-scenario baseline
#'
#' @param parPath Character model parameter path.
#' @param baselineValues Named list of baseline values from first simulation.
#' @param missingPaths Character vector of currently missing paths.
#' @param parameterValue Numeric value found in second-scenario simulation.
#' @return List with updated `baselineValues` and `missingPaths`.
#' @keywords internal
#' @noRd
.reconcileSensitivityPathValue <- function(
  parPath,
  baselineValues,
  missingPaths,
  parameterValue
) {
  valueScenario1 <- baselineValues[[parPath]]
  if (is.na(valueScenario1)) {
    baselineValues[[parPath]] <- parameterValue
    missingPaths <- setdiff(missingPaths, parPath)

    return(list(
      baselineValues = baselineValues,
      missingPaths = missingPaths
    ))
  }

  .assertSensitivityBaselineCompatibility(
    parPath = parPath,
    valueScenario1 = valueScenario1,
    valueScenario2 = parameterValue
  )

  return(list(
    baselineValues = baselineValues,
    missingPaths = missingPaths
  ))
}

#' Reconcile baseline values with a second scenario simulation
#'
#' @param sensitivityPaths Character vector of sensitivity parameter paths.
#' @param baselineValues Named list of baseline values from first simulation.
#' @param missingPaths Character vector of currently missing paths.
#' @param simulation ospsuite simulation container for second scenario.
#' @param scenarioFile Character scenario file path used for warning text.
#' @return List with updated `baselineValues` and `missingPaths`.
#' @keywords internal
#' @noRd
.reconcileSensitivityBaselineValues <- function(
  sensitivityPaths,
  baselineValues,
  missingPaths,
  simulation,
  scenarioFile
) {
  for (parPath in sensitivityPaths) {
    parameterValue <- .getSensitivityParameterValue(parPath, simulation)

    if (is.null(parameterValue)) {
      warning(
        sprintf(
          "Parameter path not found in '%s': %s",
          scenarioFile,
          parPath
        ),
        call. = FALSE
      )
      next
    }

    reconciledValues <- .reconcileSensitivityPathValue(
      parPath = parPath,
      baselineValues = baselineValues,
      missingPaths = missingPaths,
      parameterValue = parameterValue
    )
    baselineValues <- reconciledValues$baselineValues
    missingPaths <- reconciledValues$missingPaths
  }

  return(list(
    baselineValues = baselineValues,
    missingPaths = missingPaths
  ))
}

#' Convert baseline values to one-row baseline data.frame
#'
#' @param baselineValues Named list of baseline values.
#' @return One-row data.frame with available sensitivity paths.
#' @keywords internal
#' @noRd
.baselineSensitivityDataFrame <- function(baselineValues) {
  availablePaths <- names(baselineValues)[!is.na(unlist(baselineValues))]
  if (length(availablePaths) == 0) {
    stop(
      "None of the sensitivity parameter paths were found in the scenario(s)."
    )
  }

  return(data.frame(
    as.list(baselineValues[availablePaths]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))
}

#' Prepare a population for custom sensitivity analysis
#'
#' Builds a perturbed-parameter population suitable for passing to
#' `ospsuite::runSimulations`. For each entry in `sensitivityParameter`, all
#' listed model paths are multiplied by the same perturbation factor. The baseline row
#' (`perturbationFactor` = 1) is always included.
#'
#' @param scenarioFiles Named character vector (length 1 or 2) of PKML paths.
#'   Baseline values are read from the first file; if a second is given, the
#'   baseline values must agree (checked with tolerance 1e-8).
#' @param sensitivityParameter Named list; element names are sensitivity labels,
#'   elements are character vectors of model parameter paths to vary together.
#' @param variationRange Numeric fraction variation range (default `0.1`).
#' @param numberOfSteps Integer number of positive perturbation steps (default `2`).
#' @details Baseline values are read from scenario simulations. If two scenarios
#'   are provided, baseline values are cross-checked before building one
#'   perturbed population used for downstream sensitivity simulations.
#'
#' @return An ospsuite population object.
#' @keywords internal
#' @noRd
.prepareSensitivityPopulation <- function(
  scenarioFiles,
  sensitivityParameter,
  variationRange = 0.1,
  numberOfSteps = 2
) {
  checkmate::assertCharacter(scenarioFiles, min.len = 1, max.len = 2)
  checkmate::assertList(sensitivityParameter, min.len = 1)
  checkmate::assertNumber(variationRange, lower = 0)
  checkmate::assertInt(numberOfSteps, lower = 1)

  sensitivityPaths <- unique(unlist(sensitivityParameter))

  sim1 <- ospsuite::loadSimulation(
    filePath = scenarioFiles[[1]],
    loadFromCache = FALSE
  )

  baselineFromFirstScenario <- .readSensitivityBaselineValues(
    sensitivityPaths = sensitivityPaths,
    simulation = sim1,
    scenarioFile = scenarioFiles[[1]]
  )
  baselineValues <- baselineFromFirstScenario$baselineValues
  missingPaths <- baselineFromFirstScenario$missingPaths

  if (length(scenarioFiles) == 2) {
    sim2 <- ospsuite::loadSimulation(
      filePath = scenarioFiles[[2]],
      loadFromCache = FALSE
    )
    baselineFromBothScenarios <- .reconcileSensitivityBaselineValues(
      sensitivityPaths = sensitivityPaths,
      baselineValues = baselineValues,
      missingPaths = missingPaths,
      simulation = sim2,
      scenarioFile = scenarioFiles[[2]]
    )
    baselineValues <- baselineFromBothScenarios$baselineValues
    missingPaths <- baselineFromBothScenarios$missingPaths
  }

  baselineRow <- .baselineSensitivityDataFrame(baselineValues)

  return(.buildSensitivityPopulationFromBaselineRow(
    baselineRow = baselineRow,
    sensitivityParameter = sensitivityParameter,
    variationRange = variationRange,
    numberOfSteps = numberOfSteps
  ))
}
