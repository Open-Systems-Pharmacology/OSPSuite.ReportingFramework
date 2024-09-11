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

    # make sure custom params are not again overwritten by population
    scenarioList[[sc]] <- setCustomParamsToPopulation(scenarioList[[sc]])

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




#' Add user defined variability on parameters to a population from an excel file.
#'
#' @param population Object of type `Population`
#' @param XLSpath Path to the excel file that stores the information of
#'   parameters. The file must have the columns "Container.Path",
#'   "Parameter.Name", "Mean", "SD", "Units", and "Distribution". Mean and SD
#'   values must be in the base units of the parameters.
#' @param sheet Name or the index of the sheet in the excel file.
#' If `NULL`, the first sheet in the file is used.
#'
#' @details The method reads the information from the specified excel sheet(s)
#'   and calls `extendPopulationByUserDefinedParams`
#'   copy of esqlabsR::extendPopulationFromXLS but columnNames always withdot
#'
#' @export
extendPopulationFromXLS_RF <- function(population, XLSpath, sheet = NULL) {
  ospsuite.utils::validateIsOfType(population, "Population")
  ospsuite.utils::validateIsString(XLSpath)
  ospsuite.utils::validateIsString(sheet, nullAllowed = TRUE)
  if (is.null(sheet)) {
    sheet <- 1
  }

  columnNames <- c(
    "Container.Path", "Parameter.Name", "Mean",
    "SD", "Distribution"
  )

  data <- readExcel(path = XLSpath, sheet = sheet)
  names(data) <- gsub(' ','\\.',names(data))
  if (!all(columnNames %in% names(data))) {
    stop(messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames))
  }

  paramPaths <- c(dim(data)[[1]])
  meanVals <- c(dim(data)[[1]])
  sdVals <- c(dim(data)[[1]])
  distributions <- c(dim(data)[[1]])

  for (i in seq_along(data$Container.Path)) {
    paramPath <- paste(data[["Container.Path"]][[i]], data[["Parameter.Name"]][[i]], sep = "|")
    paramPaths[[i]] <- paramPath
    meanVals[[i]] <- as.numeric(data[["Mean"]][[i]])
    sdVals[[i]] <- as.numeric(data[["SD"]][[i]])
    distributions[[i]] <- data[["Distribution"]][[i]]
  }

  extendPopulationByUserDefinedParams_RF(
    population = population, parameterPaths = paramPaths,
    meanValues = meanVals, sdValues = sdVals,
    distributions = distributions
  )
}



#' Add user defined variability on parameters to a population.
#'
#' @param population Object of type `Population`
#' @param parameterPaths Vector of parameter path for which the variability is to be added.
#' @param meanValues Vector of mean values of the parameters. Must have the same
#'   length as `parameterPaths`. The type of mean (arithmetic, geometric)
#'   depends on the selected `distribution`. The values must be in the base
#'   units of the parameters.
#' @param sdValues Vector of standard deviation values of the parameters. Must
#'   have the same length as `parameterPaths`. The type of standard deviation
#'   depends on the selected `distribution`.
#' @param distributions Type of distribution from which the random values will
#'   be sampled. Must have the same length as `parameterPaths`.
#' A list of supported distributions is defined in `Distributions`. Default is `"Normal"`.
#' @export
extendPopulationByUserDefinedParams_RF <- function(population, # nolint: object_length_linter.
                                                parameterPaths,
                                                meanValues,
                                                sdValues,
                                                distributions = Distributions$Normal) {
  ospsuite.utils::validateIsOfType(population, "Population")
  ospsuite.utils::validateIsString(parameterPaths)
  ospsuite.utils::validateIsNumeric(sdValues)
  ospsuite.utils::validateIsNumeric(meanValues)
  distributions <- distributions %||% rep(Distributions$Normal, length(parameterPaths))
  ospsuite.utils::validateIsSameLength(parameterPaths, meanValues, sdValues, distributions)


  # Iterate through all parameters and sample a parameter values vector
  for (i in seq_along(parameterPaths)) {
    path <- parameterPaths[[i]]
    mean <- meanValues[[i]]
    sd <- sdValues[[i]]

    # Sample values
    vals <- sampleRandomValue(
      distribution = distributions[[i]],
      mean = mean,
      sd = sd,
      n = population$count
    )

    population$setParameterValues(parameterOrPath = path, values = vals)
  }
}
