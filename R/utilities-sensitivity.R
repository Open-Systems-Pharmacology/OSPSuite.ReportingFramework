addSensitivityTable <- function(projectConfiguration,scenarioList = NULL,scenarioName,sheetName = scenarioName){

  invisible(projectConfiguration$addAddOnfileToConfiguration(
    property = "sensitivityFile",
    value = "SensitivityParameter.xlsx",
    description = "Configuration file for Sensitivity",
    templatePath = system.file("templates","SensitivityParameter.xlsx", package = "ospsuite.reportingframework")
  ))

  if (!is.null(scenarioList)){

    checkmate::assertChoice(scenarioName,choices = names(scenarioList))
    checkmate::assertString(sheetName,max.chars = 31)

    parameterPaths <- ospsuite::potentialVariableParameterPathsFor(scenarioList[[scenarioName]]$simulation)

    dt = data.table(parameter = gsub('\\|',' ',parameterPaths),
                    parameterPath = parameterPaths)

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

runSensitivityAnalysisForScenarios <-
  function(projectConfiguration,
           scenarioList,
           scenarioNames = NULL,
           sensitivitysheet,
           sensitivityAnalysisRunOptions = SensitivityAnalysisRunOptions$new(showProgress = TRUE)) {

  if (!('sensitivityFile' %in% names(projectConfiguration$addOns)))
    stop(
      "SensitivityParameter xlsx is not added to the projectConfiguration Please call 'addSensitivityTable(projectConfiguration)'"
    )

    outputFolder <- file.path(projectConfiguration$outputFolder,EXPORTDIR$sensitivityResults)
    if (!dir.exists(outputFolder)) dir.create(outputFolder)

    dtScenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  sensitivityParameterDt <- xlsxReadData(projectConfiguration$addOns$sensitivityFile,sheetName = sensitivitysheet)

  for (scenarioName in scenarioNames){

    pkParameterSheets <- dtScenarios[scenarioName == scenarioName & !is.na(pKParameter)]$pKParameter
    if (length(pkParameterSheets) > 0){
      initializeParametersOfSheets(projectConfiguration,pkParameterSheets)

      sensitivityAnalysis <- SensitivityAnalysis$new(
        simulation = scenarioList[[scenarioName]]$simulation,
        parameterPaths = sensitivityParameterDt$parameterPath)

      sensitivityResults <-
        ospsuite::runSensitivityAnalysis(sensitivityAnalysis = sensitivityAnalysis,
                                         sensitivityAnalysisRunOptions = sensitivityAnalysisRunOptions)

      exportSensitivityAnalysisResultsToCSV(
        results = sensitivityResults,
        filePath = file.path(outputFolder,sensitivityAnalyisName(scenarioName,sensitivitysheet)
        )
      )
    }

  }


  }

sensitivityAnalyisName <- function(scenarioName,sensitivitysheet){
  paste0(scenarioName, '_', sensitivitysheet, '.csv')
}


