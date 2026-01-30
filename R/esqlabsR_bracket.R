#' Initialize Project Directory
#'
#' This function initializes a project directory by creating necessary subdirectories
#' and copying configuration files from a source Excel file. It reads the directory
#' structure and files specified in the provided Excel template and sets up the project
#' environment accordingly.
#'
#' @param configurationDirectory A character string specifying the path to the project
#' directory to be initialized. Defaults to the current working directory ('.').
#'
#' @param sourceConfigurationXlsx A character string representing the path to the source
#' Excel file containing the project configuration. By default, it uses the template
#' provided by the 'ospsuite.reportingframework' package.
#'
#' @param templatePath path of all template files
#'
#' @param overwrite A logical value indicating whether to overwrite existing files in the
#' project directory. Defaults to FALSE, meaning existing files will not be overwritten.
#'
#' @return This function returns an invisible NULL. It is used for its side effects of
#' creating directories and copying files rather than producing a value.
#'
#'
#' @export
#' @family project initialization
initProject <- function(configurationDirectory = ".",
                        sourceConfigurationXlsx = system.file("templates", "ProjectConfiguration.xlsx", package = "ospsuite.reportingframework"),
                        templatePath = system.file("templates", package = "ospsuite.reportingframework"),
                        overwrite = FALSE) {
  configurationDirectory <- fs::path_abs(configurationDirectory)

  checkmate::assertFileExists(sourceConfigurationXlsx)
  checkmate::assertDirectoryExists(templatePath)

  file.copy(
    from = sourceConfigurationXlsx,
    to = file.path(configurationDirectory, basename(sourceConfigurationXlsx)),
    overwrite = overwrite
  )

  dt <- xlsxReadData(sourceConfigurationXlsx)
  filesAvailable <- list.files(templatePath)

  filesToCopy <- intersect(dt$value, filesAvailable) %>% unique()

  dirsToCreate <- setdiff(dt$value, c(filesToCopy)) %>%
    unique()

  for (d in dirsToCreate) {
    dabsolute <- fs::path_abs(d, start = configurationDirectory)
    if (!dir.exists(dabsolute)) {
      dir.create(dabsolute, recursive = TRUE, showWarnings = FALSE)
    }
  }

  for (f in filesToCopy) {
    fabsolute <- fs::path_abs(f, start = configurationDirectory)
    if (!file.exists(fabsolute) | overwrite) {
      file.copy(
        from = file.path(templatePath, f),
        to = fabsolute,
        overwrite = overwrite
      )
    }
  }

  return(invisible())
}

#' #' Create a `ProjectConfiguration`
#'
#' @description  Create a `ProjectConfigurationRF` based on the `"ProjectConfiguration.xlsx"`
#'
#' based on esqlabsR::ProjectConfiguration but with additional file information for PK Parameter definitions
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. default to the `ProjectConfiguration.xlsx` file located in the working directory.
#'
#' @return Object of type `ProjectConfigurationRF`
#' @export
#' @family project initialization
createProjectConfiguration <- function(path = file.path("ProjectConfiguration.xlsx")) {
  projectConfiguration <- ProjectConfigurationRF$new(projectConfigurationFilePath = path)

  return(projectConfiguration)
}
#' Fix file paths in scenario configurations by replacing dash variants with standard hyphen-minus
#'
#' This function checks if all files referenced in scenario configurations exist.
#' If a file is not found, it tries replacing various dash unicode characters
#' with the standard hyphen-minus character (U+002D). This addresses issues where
#' LibreOffice converts standard hyphens to other unicode variants (e.g., EN DASH
#' U+2013, EM DASH U+2014) when saving Excel files.
#'
#' @param scenarioConfigurations List of scenario configuration objects from
#'   `esqlabsR::readScenarioConfigurationFromExcel()`
#' @param projectConfiguration Object of class `ProjectConfiguration` containing
#'   information on paths and file names
#'
#' @return The scenarioConfigurations list with corrected file paths
#' @keywords internal
#' @noRd
.fixFilePathsInScenarioConfigurations <- function(scenarioConfigurations,
                                                 projectConfiguration) {
  # Define unicode dash characters that might be mistaken for standard hyphen-minus
  # U+002D: HYPHEN-MINUS (standard keyboard character)
  # U+2010: HYPHEN
  # U+2011: NON-BREAKING HYPHEN
  # U+2012: FIGURE DASH
  # U+2013: EN DASH (commonly inserted by LibreOffice)
  # U+2014: EM DASH
  # U+2015: HORIZONTAL BAR
  dashVariants <- c("\u2010", "\u2011", "\u2012", "\u2013", "\u2014", "\u2015")

  # Process each scenario configuration
  for (i in seq_along(scenarioConfigurations)) {
    scenarioConfig <- scenarioConfigurations[[i]]

    # Validate that modelFile exists and is not NULL or empty
    if (is.null(scenarioConfig$modelFile) || nchar(scenarioConfig$modelFile) == 0) {
      stop(paste0(
        "Invalid scenario configuration for scenario '", scenarioConfig$scenarioName, "': ",
        "modelFile is ", if (is.null(scenarioConfig$modelFile)) "NULL" else "empty", ". ",
        "All scenarios must have a non-empty modelFile with .pkml extension."
      ))
    }

    modelFile <- scenarioConfig$modelFile
    modelPath <- file.path(projectConfiguration$modelFolder, modelFile)

    # Check if file exists
    if (!file.exists(modelPath)) {
      # Try replacing each dash variant with standard hyphen-minus
      correctedFile <- modelFile
      for (dashVariant in dashVariants) {
        correctedFile <- gsub(dashVariant, "-", correctedFile, fixed = TRUE)
      }

      correctedPath <- file.path(projectConfiguration$modelFolder, correctedFile)

      # If corrected path exists, update the configuration
      if (file.exists(correctedPath)) {
        writeToLog(
          type = "Warning",
          msg = paste0(
            "File '", modelFile, "' not found. ",
            "Using corrected filename '", correctedFile, "' instead. ",
            "Consider updating the scenario configuration file to use standard hyphens (-)."
          )
        )
        scenarioConfigurations[[i]]$modelFile <- correctedFile
      } else {
        # File not found even after correction
        stop(paste0(
          "Model file not found for scenario '", scenarioConfig$scenarioName, "': ",
          "Neither '", modelFile, "' nor '", correctedFile, "' exists in '",
          projectConfiguration$modelFolder, "'. ",
          "Please check the file name in the scenario configuration."
        ))
      }
    }
  }

  return(scenarioConfigurations)
}
#' Create Scenario objects from `ScenarioConfiguration` objects
#'
#' wrap of `esqlabsR::createDefaultProjectConfiguration()` with `esqlabsR::createScenarios()` as input
#'
#' @param projectConfiguration Object of class `ProjectConfiguration` containing information on paths and file names
#' @param scenarioNames Names of the scenarios that are defined in the excel file.
#' If NULL (default), all scenarios specified in the excel file will be created.
#'
#' @return  Named list of Scenario objects.
#' @export
#' @family scenario management
createScenarios.wrapped <- function(projectConfiguration, # nolint
                                    scenarioNames = NULL) {
  # Read scenario configurations from Excel
  scenarioConfigurations <- esqlabsR::readScenarioConfigurationFromExcel(
    scenarioNames = scenarioNames,
    projectConfiguration = projectConfiguration
  )

  # Check and fix file paths with hyphen/dash issues
  scenarioConfigurations <- .fixFilePathsInScenarioConfigurations(
    scenarioConfigurations = scenarioConfigurations,
    projectConfiguration = projectConfiguration
  )

  # Create scenarios with fixed configurations
  scenarioList <- esqlabsR::createScenarios(
    scenarioConfigurations = scenarioConfigurations
  )

  synchronizeScenariosWithPlots(projectConfiguration)
  synchronizeScenariosOutputsWithPlots(projectConfiguration)

  return(scenarioList)
}
#' Load existing scenario results
#'
#' This function loads the results of specified scenarios. If the results do not exist,
#' it returns an error.
#'
#' @param projectConfiguration Configuration for the project, containing paths and settings necessary
#' to load the results.
#' @param scenarioNames Character vector of the names of the scenarios whose results are to be loaded.
#'
#' @return A list containing the loaded scenario results, including population data if available.
#' throws Error if the scenario results do not exist.
#'
#' @export
#' @family scenario management
loadScenarioResultsToFramework <- function(projectConfiguration, scenarioNames) {
  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult)
  resultFiles <- file.path(outputFolder, paste0(scenarioNames, ".csv"))

  if (!all(file.exists(resultFiles))) {
    stop(paste(
      "Error: Simulation results for scenario(s)",
      paste(scenarioNames[!file.exists(resultFiles)], collapse = ", "),
      "do not exist."
    ))
  }

  scenarioResults <- list()

  for (sc in scenarioNames) {
    writeToLog(type = "Info", msg = paste("Load simulation result of", sc))

    scenarioResult <- esqlabsR::loadScenarioResults(
      scenarioNames = sc,
      resultsFolder = outputFolder
    )[[1]]

    # Load population if it exists
    popFile <- file.path(outputFolder, paste0(sc, "_population.csv"))
    if (file.exists(popFile)) {
      scenarioResult[["population"]] <- ospsuite::loadPopulation(popFile)
    }

    scenarioResults[[sc]] <- scenarioResult
  }

  return(scenarioResults)
}
#' Run and save scenarios
#'
#' This function simulates a list of scenarios and saves the results.
#' If results already exist for a scenario, it will overwrite them based on the provided options.
#'
#' @param projectConfiguration Configuration for the project, containing paths and settings necessary
#' to run the simulations and save the results.
#' @param scenarioList Named list of Scenario objects to be simulated.
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will be passed to simulation runs.
#' If `NULL`, default options are used.
#' @param ... Additional arguments passed to `esqlabsR::saveScenarioResults`.
#'
#' @return A list containing the simulation results for each scenario that was run.
#'
#' @examples
#' \dontrun{
#' runAndSaveScenarios(
#'   projectConfiguration = myProjectConfig,
#'   scenarioList = myScenarioList,
#'   simulationRunOptions = myRunOptions
#' )
#' }
#'
#' @export
#' @family scenario management
runAndSaveScenarios <- function(projectConfiguration, scenarioList, simulationRunOptions = NULL, ...) {
  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult)

  scenarioResults <- list()

  for (sc in names(scenarioList)) {
    writeToLog(type = "Info", msg = paste("Start simulation of", sc))

    # Make sure custom params are not again overwritten by population
    scenarioList[[sc]] <- .setCustomParamsToPopulation(scenarioList[[sc]])

    scenarioResults[sc] <- esqlabsR::runScenarios(
      scenarios = scenarioList[sc],
      simulationRunOptions = simulationRunOptions
    )

    # Set scenario name as simulation name
    scenarioResults[[sc]]$simulation$set("Name", sc)

    esqlabsR::saveScenarioResults(
      simulatedScenariosResults = scenarioResults[sc],
      projectConfiguration = projectConfiguration,
      outputFolder = outputFolder,
      ...
    )
  }
  calculatePKParameterForScenarios(projectConfiguration, scenarioResults)

  return(invisible(scenarioResults))
}

#' Run or load scenarios
#'
#' This function checks if the simulation results for scenarios already exist.
#' If they do, it loads them; otherwise, it runs the scenarios and saves the results.
#'
#' @param projectConfiguration Configuration for the project, containing paths and settings necessary
#' to run the simulations and load the results.
#' @param scenarioList Named list of Scenario objects to be managed.
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will be passed to simulation runs.
#' If `NULL`, default options are used.
#' @param ... Additional arguments passed to `runAndSaveScenarios`.
#'
#' @return A list containing the simulation results for each scenario that was loaded or run.
#'
#' @export
#' @family scenario management
runOrLoadScenarios <- function(projectConfiguration, scenarioList, simulationRunOptions = NULL, ...) {
  scenarioResults <- list()

  for (sc in names(scenarioList)) {
    if (file.exists(file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv")))) {
      scenarioResults[sc] <- loadScenarioResultsToFramework(projectConfiguration, sc)
    } else {
      scenarioResults[sc] <- runAndSaveScenarios(projectConfiguration, scenarioList[sc], simulationRunOptions, ...)
    }
  }

  return(invisible(scenarioResults))
}
#' Read Ontogenies from Data
#'
#' based on esqlabsR:::.readOntongeniesFromXLS
#'
#' This function extracts protein ontogeny mappings from the provided data.
#' It splits the mappings into individual protein-ontogeny pairs and validates
#' the structure of each pair. Each valid pair is then converted into a
#' `MoleculeOntogeny` object.
#'
#' @param data A data frame containing a column named "Protein Ontogenies".
#'
#' @return A list of `MoleculeOntogeny` objects, each representing a protein
#' and its corresponding ontogeny. Returns NULL if the "Protein Ontogenies"
#' field is NA.
#'
#' @keywords internal
#' @noRd
.readOntongenies <- function(data) {
  proteinOntogenyMappings <- data[["protein Ontogenies"]]
  if (is.na(proteinOntogenyMappings)) {
    return(NULL)
  }
  proteinOntogenyMappings <- as.character(proteinOntogenyMappings)
  proteinOntogenyMappings <- unlist(strsplit(
    x = proteinOntogenyMappings,
    split = ",", fixed = TRUE
  ))
  proteinOntogenyMappings <- trimws(proteinOntogenyMappings)
  moleculeOntogenies <- vector("list", length(proteinOntogenyMappings))
  for (i in seq_along(proteinOntogenyMappings)) {
    ontogeny <- proteinOntogenyMappings[[i]]
    ontogenyMapping <- unlist(strsplit(
      x = ontogeny, split = ":",
      fixed = TRUE
    ))
    if (length(ontogenyMapping) != 2) {
      stop(paste("The ontogeny has the wrong structure:", ontogeny))
    }
    protein <- ontogenyMapping[[1]]
    ontogeny <- ontogenyMapping[[2]]
    ospsuite.utils::validateEnumValue(value = ontogeny, enum = ospsuite::StandardOntogeny)
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }
  return(moleculeOntogenies)
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
#' @keywords internal
#' @noRd
.extendPopulationFromXLS_RF <- function(population, XLSpath, sheet = NULL) { # nolint
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
  names(data) <- gsub(" ", "\\.", names(data))
  if (!all(columnNames %in% names(data))) {
    stop("errorWrongXLSStructure")
    # stop(messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames)) # nolint
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

  .extendPopulationByUserDefinedParams_RF(
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
#' @keywords internal
#' @noRd
.extendPopulationByUserDefinedParams_RF <- function(population, # nolint
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
