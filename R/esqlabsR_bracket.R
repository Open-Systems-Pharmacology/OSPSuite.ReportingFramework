#' Initialize Project
#'
#' @description
#'
#' Creates the default project folder structure with excels file templates in
#' the working directory.
#'
#' @param projectPath A string defining the path where to initialize the project.
#'  default to current working directory.
#' @param sourceFolder path of template directory available is
#'    `templateDirectory()` default path of `ospsuite.reportingframework`
#'    `esqlabsR:::example_directory("TestProject")`  default path for Esqlabs-projects
#' @param overwrite A boolean, if TRUE existing files will be overwritten
#'
#' @returns path of project
#'
#' @export
initProject <- function(projectPath = ".",
                        sourceFolder,
                        overwrite = FALSE) {
  projectPath <- fs::path_abs(projectPath)

  checkmate::assertDirectoryExists(sourceFolder)


  dirsToCopy <- fs::path_rel(path = list.dirs(file.path(sourceFolder)), start = sourceFolder)

  for (d in dirsToCopy) {
    if (!dir.exists(file.path(projectPath, d))) {
      dir.create(file.path(projectPath, d), recursive = TRUE, showWarnings = FALSE)
    }

    fileList <- fs::path_rel(path = fs::dir_ls(file.path(sourceFolder, d), type = "file"), start = sourceFolder)

    for (f in fileList) {
      if (!file.exists(file.path(projectPath, f)) | overwrite) {
        file.copy(
          from = file.path(sourceFolder, f),
          to = file.path(projectPath, f),
          overwrite = overwrite
        )
      }
    }
  }

  return(projectPath)
}


#' Create a default `ProjectConfiguration`
#'
#' wrap of 'esqlabsR::createDefaultProjectConfiguration()'
#'
#' @param path Full path of an XLS/XLSX file
#'
#' @return Object of type ProjectConfiguration
#'
#' @export
createDefaultProjectConfiguration.wrapped <- function(path) { # nolint
  logCatch({
    projectConfiguration <- esqlabsR::createDefaultProjectConfiguration(path = path)

    message(paste(utils::capture.output(projectConfiguration), collapse = "\n"))
  })

  return(projectConfiguration)
}


#' Create Scenario objects from `ScenarioConfiguration` objects
#'
#' wrap of `esqlabsR::createDefaultProjectConfiguration()` with `esqlabsR::createScenarios()` as input
#'
#' @param projectConfiguration A `ProjectConfiguration` object holding base information
#' @param scenarioNames Names of the scenarios that are defined in the excel file.
#' If NULL (default), all scenarios specified in the excel file will be created.
#'
#' @return  Named list of Scenario objects.
#' @export
createScenarios.wrapped <- function(projectConfiguration, # nolint
                                    scenarioNames = NULL) {
  logCatch({
    scenarioList <-
      esqlabsR::createScenarios(
        esqlabsR::readScenarioConfigurationFromExcel(
          scenarioNames = scenarioNames,
          projectConfiguration = projectConfiguration
        )
      )
  })
  return(scenarioList)
}

#' Run a set of scenarios.
#'
#' wrap of `esqlabsR::runScenarios`
#'
#' @param scenarioList  Named list of Scenario objects.
#' @param ... passed to esqlabsR::runScenarios
#'
#' @return  Named list of simulation results
#' @export
runScenarios.wrapped <- function(scenarioList, ...) { # nolint
  logCatch(
    scenarioResults <- esqlabsR::runScenarios(scenarios = scenarioList, ...)
  )

  return(scenarioResults)
}

#' Save results of scenario simulations to .csv file
#'
#' wrap of `esqlabsR::saveScenarioResults`
#'
#' @param simulatedScenariosResults Named list with `simulation`, `results`, `outputValues`, and `population` as produced by runScenarios()
#' @param projectConfiguration An instance of ProjectConfiguration
#' @param ... arguments passed to esqlabsR::saveScenarioResults
#'
#' @export
saveScenarioResults.wrapped <- function(simulatedScenariosResults, # nolint
                                        projectConfiguration, ...) {
  logCatch({
    outputFolder <- file.path(projectConfiguration$outputFolder, "SimulationResults")
    esqlabsR::saveScenarioResults(
      simulatedScenariosResults = simulatedScenariosResults,
      projectConfiguration = projectConfiguration,
      outputFolder = outputFolder,
      ...
    )
  })
}
