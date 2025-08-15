#' Build Test Project Directory
#'
#' This function sets up a test project directory for the `ospsuite.reportingframework` package.
#' It initializes the project structure, configures scenarios, and handles necessary file
#' synchronization for testing and development purposes. It can be used in both development
#' mode and testing environments.
#'
#' @param rootDirectory A character string specifying the root directory for the test project.
#'                     If NULL (default), a temporary directory will be created.
#' @param verbose A logical value indicating whether to print messages about the process.
#'                Defaults to FALSE.
#' @param writeTestData A logical value indicating whether newly created testdata should be filed to the inst directory of the package.
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @details
#' The function performs the following steps:
#' 1. Initializes the project structure.
#' 2. Cleans up existing data and sets up mock manual edits on project configuration.
#' 3. Synchronizes directories for models and populations.
#' 4. Exports defined populations and runs simulations for scenarios.
#' 5. In development mode, it synchronizes simulation results and PK analysis results back
#'    to the development directory.
#'
#' @examples
#' \dontrun{
#' # Example usage in package development
#' if (writeTestData) {
#'   testProject <- buildTestProjectory(verbose = TRUE, writeTestData = TRUE)
#' }
#'
#' # Example usage in setup.R for tests
#' testSetup <- buildTestProjectory(rootDirectory = "path/to/testdirectory", verbose = TRUE)
#' projectdirectory <- testSetup$projectConfiguration
#' scenarioList <- testSetup$scenarioList
#'
#' # Example usage in vignettes
#' vignetteSetup <- buildTestProjectory(verbose = TRUE)
#' # Use vignetteSetup$projectConfiguration and vignetteSetup$scenarioList as needed
#' }
#'
#' @export
buildTestData <- function(rootDirectory = NULL,
                          writeTestData = FALSE) {
  # Initialize class to build test project
  pBuilder <- TestProjectBuilder$new()

  # get projectConfiguration
  projectConfiguration <- pBuilder$iniTestProject(rootDirectory = rootDirectory)

  # Clear existing data from relevant Excel sheets
  pBuilder$mockManualEditingsCleanup(projectConfiguration)

  # Define virtual populations within biometric ranges
  randomPops <- data.table(
    populationName = c("adults", "toddler", "children", "school-children", "adolescents"),
    species = "Human",
    population = "European_ICRP_2002",
    numberOfIndividuals = 100,
    proportionOfFemales = 50,
    ageMin = c(20, 0.5, 2, 6, 12),
    ageMax = c(40, 2, 6, 12, 18),
    weightUnit = "kg",
    heightUnit = "cm",
    bMIUnit = "kg/m\u00B2",
    protein = "CYP3A4,UGT1A4",
    ontogeny = "CYP3A4,UGT1A4"
  )
  pBuilder$mockManualEditingsPopulation(projectConfiguration,
    randomPops = randomPops
  )

  # Set up scenarios based on the defined populations, add PK Parameter and output paths
  pBuilder$mockManualEditingsScenario(projectConfiguration,
    dtTestScenarios = getTestScenarios(projectConfiguration),
    pKParameter = "PK_Plasma, PK_Fraction"
  )

  # Add PK parameters to the project configuration
  pBuilder$mockManualEditingsPKParameter(projectConfiguration)

  pBuilder$setupRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = unique(randomPops$populationName),
    writeTestData = writeTestData
  )

  # Initialize all scenarios previously defined in scenario.xlsx
  scenarioList <- createScenarios.wrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = NULL
  )

  # Run scenarios and calculate PK
  scenarioResults <- pBuilder$setupSimulations(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    writeTestData = writeTestData
  )

  # generate Random data
  setupRandomDataForTest(
    pBuilder = pBuilder,
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    scenarioResults = scenarioResults
  )

  # generate virtual twin populations
  scenarioListInd <-
    setupVirtualTwinScenariosForTest(
      pBuilder = pBuilder,
      projectConfiguration = projectConfiguration
    )

  # Run scenarios and calculate PK
  scenarioResultsInd <- pBuilder$setupSimulations(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioListInd,
    writeTestData = writeTestData
  )

  # Setup sensitivity
  projectConfiguration <- pBuilder$setUpSensitivity(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioListInd,
    scenarioName = names(scenarioListInd)[2],
    parameterList = list(
      "DrugX Lipophilicity" = "DrugX Lipophilicity",
      "DrugX Fraction unbound" = "DrugX Fraction unbound",
      "CYP3A4 Ontogeny factor" = "CYP3A4 Ontogeny factor"
    ),
    sensitivitySheet = "smallSelection",
    writeTestData = writeTestData
  )

  return(invisible(list(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    scenarioResults = scenarioResults,
    scenarioListInd = scenarioListInd,
    scenarioResultsInd = scenarioResultsInd
  )))
}

## auxiliaries --------
#' Setup Random Data for Test
#'
#' This function adds random pharmacokinetic (PK and time profile) data to the test project,
#' including shifting values for selected individuals.
#'
#' @param pBuilder An TestProjectBuilder object, which provides function to build the test project
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @param scenarioList A list of scenarios initialized for the project.
#' @param scenarioResults A list containing results from running the scenarios.
#'
#' @return Invisible NULL.
#' @keywords internal
setupRandomDataForTest <- function(pBuilder, projectConfiguration, scenarioList, scenarioResults) {
  pkParameterDT <- loadPKParameter(
    projectConfiguration = projectConfiguration,
    scenarioListOrResult = scenarioList
  )

  set.seed(123) # Set seed for reproducibility
  ids <- sample(unique(pkParameterDT$individualId), 6, replace = FALSE)

  pBuilder$addRandomTPData(
    projectConfiguration = projectConfiguration,
    scenarioResults = scenarioResults[c("adults_iv", "adults_po")],
    ids = ids
  )

  pBuilder$addRandomPKData(
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT,
    ids = ids
  )

  return(invisible())
}

getTestScenarios <- function(projectConfiguration) {
  # initialize variable to avoid messages
  scenario_name <- NULL #nolint
  longName <- shortName <- outputPathsIds <- NULL

  instDirectory <- system.file(
    package = "ospsuite.reportingframework",
    "extdata",
    mustWork = TRUE
  )

  modelFiles <- list.files(file.path(instDirectory, "Models"))
  names(modelFiles) <- substr(modelFiles, 1, 2)

  dtPop <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")

  dtScenario <- rbind(
    data.table(
      scenario_name = paste0(gsub("-", "_", dtPop$populationName), "_iv"),
      populationId = dtPop$populationName,
      readPopulationFromCSV = 1,
      modelFile = modelFiles["iv"]
    ),
    data.table(
      scenario_name = paste0(gsub("-", "_", dtPop$populationName), "_po"),
      populationId = dtPop$populationName,
      readPopulationFromCSV = 1,
      modelFile = modelFiles["po"]
    ),
    fill = TRUE
  )

  dtScenario[, outputPathsIds := "Plasma, CYP3A4total, CYP3A4Liver"]
  dtScenario[, shortName := gsub("_", " ", gsub("_iv", "", gsub("_po", "", scenario_name)))]
  dtScenario[, longName := gsub("_", " ", gsub("_iv", "", gsub("_po", "", scenario_name)))]

  return(dtScenario)
}


#' Setup Virtual Twin Scenarios for Test
#'
#' This function sets up virtual twin scenarios for the test project based on the defined populations
#' and model files.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#'
#' @return A list of scenario names initialized for the virtual twin scenarios.
#' @keywords internal
setupVirtualTwinScenariosForTest <- function(pBuilder, projectConfiguration) {
  # initialize variable to avoid messages
  modelFile <- readPopulationFromCSV <- NULL
  scenario_name <- NULL #nolint

  invisible(readObservedDataByDictionary(projectConfiguration))

  dtVirtualTwin <- pBuilder$mockManualEditingsIndividuals(projectConfiguration)

  individualIds <- dtVirtualTwin$individualId

  # add a scenarios
  dtTestScenarios <- rbind(
    data.table(
      scenario_name = "p_1234_adults_iv",
      populationId = "1234_adults",
      readPopulationFromCSV = 1,
      individualId = ""
    ),
    data.table(
      scenario_name = paste(tolower(individualIds), "iv", sep = "_"),
      populationId = "",
      readPopulationFromCSV = 0,
      individualId = individualIds
    )
  )
  dtTestScenarios[readPopulationFromCSV == 1, `:=`(
    shortName = "study 1234",
    longName = "study 1234 iv application"
  )]
  dtTestScenarios[readPopulationFromCSV == 0, `:=`(
    shortName = paste("individual", gsub("i1234", "", gsub("_iv", "", scenario_name))),
    longName = paste("individual", gsub("i1234", "", gsub("_iv", "", scenario_name)), "iv application")
  )]

  modelFiles <- list.files(projectConfiguration$modelFolder, pattern = ".pkml")
  dtTestScenarios[grep("_iv$", scenario_name), modelFile := grep("^iv", modelFiles, value = TRUE)]
  dtTestScenarios[grep("_po$", scenario_name), modelFile := grep("^po", modelFiles, value = TRUE)]

  pBuilder$mockManualEditingsScenario(
    projectConfiguration = projectConfiguration,
    dtTestScenarios = dtTestScenarios
  )

  exportVirtualTwinPopulations(
    projectConfiguration = projectConfiguration,
    modelFile = list.files(projectConfiguration$modelFolder, pattern = ".pkml")[1],
    overwrite = TRUE
  )


  scenarioListInd <- suppressWarnings(createScenarios.wrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = dtTestScenarios$scenario_name
  ))

  return(scenarioListInd)
}
