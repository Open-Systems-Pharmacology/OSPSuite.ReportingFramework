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

    wb <- addDataAsTemplateToXlsx(
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
#' Defaults to an instance of SensitivityAnalysisRunOptions with showProgress set to TRUE.
#' @param overwrite A logical indicating whether to overwrite existing results. Defaults to TRUE.
#'
#' @return NULL
#' @export
runSensitivityAnalysisForScenarios <-
  function(projectConfiguration,
           scenarioList,
           scenarioNames = NULL,
           sensitivitysheet,
           sensitivityAnalysisRunOptions = SensitivityAnalysisRunOptions$new(showProgress = TRUE),
           overwrite = TRUE) {
    if (!("sensitivityFile" %in% names(projectConfiguration$addOns))) {
      stop(
        "SensitivityParameter xlsx is not added to the projectConfiguration Please call 'addSensitivityTable(projectConfiguration)'"
      )
    }

    outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$sensitivityResults)
    if (!dir.exists(outputFolder)) dir.create(outputFolder)

    dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

    sensitivityParameterDt <- xlsxReadData(projectConfiguration$addOns$sensitivityFile, sheetName = sensitivitysheet)

    for (scenarioName in scenarioNames) {
      if (!file.exists(file.path(outputFolder, sensitivityAnalyisName(scenarioName, sensitivitysheet))) |
        overwrite) {
        pkParameterSheets <- dtScenarios[scenarioName == scenarioName & !is.na(pKParameter)]$pKParameter
        if (length(pkParameterSheets) > 0) {
          initializeParametersOfSheets(projectConfiguration, pkParameterSheets)

          sensitivityAnalysis <- SensitivityAnalysis$new(
            simulation = scenarioList[[scenarioName]]$simulation,
            parameterPaths = sensitivityParameterDt$parameterPath
          )

          sensitivityResults <-
            ospsuite::runSensitivityAnalysis(
              sensitivityAnalysis = sensitivityAnalysis,
              sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions
            )

          exportSensitivityAnalysisResultsToCSV(
            results = sensitivityResults,
            filePath = file.path(outputFolder, sensitivityAnalyisName(scenarioName, sensitivitysheet))
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
sensitivityAnalyisName <- function(scenarioName, sensitivitysheet) {
  paste0(scenarioName, "_", sensitivitysheet, ".csv")
}
