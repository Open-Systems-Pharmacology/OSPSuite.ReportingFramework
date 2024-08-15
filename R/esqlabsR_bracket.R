#' Initialize Project
#'
#' @description
#'
#' Creates the default project folder structure with excels file templates in
#' the working directory.
#'
#' @param rootDirectory A string defining the path where to initialize the project.
#'  default to current working directory.
#' @param sourceFolder path of template directory available is
#'    `templateDirectory()` default path of `ospsuite.reportingframework`
#'    `esqlabsR:::example_directory("TestProject")`  default path for Esqlabs-projects
#' @param overwrite A boolean, if TRUE existing files will be overwritten
#'
#' @export
initProject <- function(rootDirectory = "..",
                        sourceFolder,
                        overwrite = FALSE) {
  rootDirectory <- fs::path_abs(rootDirectory)

  checkmate::assertDirectoryExists(sourceFolder)


  dirsToCopy <- fs::path_rel(path = list.dirs(file.path(sourceFolder)), start = sourceFolder)

  for (d in dirsToCopy) {
    if (!dir.exists(file.path(rootDirectory, d))) {
      dir.create(file.path(rootDirectory, d), recursive = TRUE, showWarnings = FALSE)
    }

    fileList <- fs::path_rel(path = fs::dir_ls(file.path(sourceFolder, d), type = "file"), start = sourceFolder)

    for (f in fileList) {
      if (!file.exists(file.path(rootDirectory, f)) | overwrite) {
        file.copy(
          from = file.path(sourceFolder, f),
          to = file.path(rootDirectory, f),
          overwrite = overwrite
        )
      }
    }
  }

  return(invisible())
}


#' Create Scenario objects from `ScenarioConfiguration` objects
#'
#' wrap of `esqlabsR::createDefaultProjectConfiguration()` with `esqlabsR::createScenarios()` as input
#'
#' @template projectConfig
#' @param scenarioNames Names of the scenarios that are defined in the excel file.
#' If NULL (default), all scenarios specified in the excel file will be created.
#' @param doCheckScenarioNameValidity `boolean` If TRUE scenario names will be check if they
#'  can be used as file names in an electronic package
#'
#' @return  Named list of Scenario objects.
#' @export
createScenarios.wrapped <- function(projectConfiguration, # nolint
                                    scenarioNames = NULL,
                                    doCheckScenarioNameValidity = TRUE) {
  if (doCheckScenarioNameValidity) checkScenarioNameValidity(projectConfiguration)

  scenarioList <-
    esqlabsR::createScenarios(
      esqlabsR::readScenarioConfigurationFromExcel(
        scenarioNames = scenarioNames,
        projectConfiguration = projectConfiguration
      )
    )

  return(scenarioList)
}

#' Run a set of scenarios.
#'
#' uses `esqlabsR::runScenarios` and `esqlabsR::saveScenarioResults
#' runs the simulations and save the result`
#'
#' @template projectConfig
#' @param scenarioList  Named list of Scenario objects.
#' @param simulationRunOptions Object of type SimulationRunOptions that will be passed to simulation runs.
#' If NULL, default options are used.
#' @param ... arguments passed to esqlabsR::saveScenarioResults
#'
#' @export
runAndSaveScenarios <- function(projectConfiguration,
                                scenarioList,
                                simulationRunOptions = NULL,
                                ...) { # nolint

  outputFolder <- file.path(projectConfiguration$outputFolder, "SimulationResults")

  for (sc in names(scenarioList)) {
    message(paste("Start simulation of", sc))

    scenarioResults <- esqlabsR::runScenarios(scenarios = scenarioList[sc], ...)

    esqlabsR::saveScenarioResults(
      simulatedScenariosResults = scenarioResults,
      projectConfiguration = projectConfiguration,
      outputFolder = outputFolder,
      ...
    )
  }

  return(invisible())
}
