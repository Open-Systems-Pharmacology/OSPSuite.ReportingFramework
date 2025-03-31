#' Run Example 1 of the Reporting Framework
#'
#' This function sets up the environment for running Example 1 of the
#' Reporting Framework, following the workflow outlined in the tutorial 'Tutorial-timeprofiles'.
#' It creates a directory structure if it does not exist, and sources
#' the necessary R scripts to execute the workflow.
#'
#' @param rootDirectory A character string specifying the root directory
#'                      where the example files will be created. If NULL,
#'                      a temporary directory will be used.
#'
#' @return A character string indicating the path to the root directory
#'         where the example files were created.
#' @export
runExample1 <- function(rootDirectory = NULL) {
  library(data.table)
  library(tidyr)

  if (is.null(rootDirectory)) {
    rootDirectory <- tempdir()
  }
  message(rootDirectory)
  if (!dir.exists(file.path(rootDirectory, "Scripts", "ReportingFramework"))) {
    dir.create(file.path(rootDirectory, "Scripts", "ReportingFramework"), recursive = TRUE)
  }

  setwd(file.path(rootDirectory, "Scripts", "ReportingFramework"))

  source(system.file(
    "extdata", "example_1", "workflow.R",
    package = "ospsuite.reportingframework",
    mustWork = TRUE
  ))

  return(rootDirectory)
}
#' Run Example 2 of the Reporting Framework
#'
#' This function sets up the environment for running Example 2 of the
#' Reporting Framework,
#'
#' @param rootDirectory A character string specifying the root directory
#'                      where the example files will be created. If NULL,
#'                      a temporary directory will be used.
#'
#' @return A character string indicating the path to the root directory
#'         where the example files were created.
#' @export
runExample2 <- function(rootDirectory = NULL) {
  if (is.null(rootDirectory)) {
    rootDirectory <- tempdir()
  }
  message(rootDirectory)
  if (!dir.exists(file.path(rootDirectory, "Scripts", "ReportingFramework"))) {
    dir.create(file.path(rootDirectory, "Scripts", "ReportingFramework"), recursive = TRUE)
  }

  setwd(file.path(rootDirectory, "Scripts", "ReportingFramework"))

  source(system.file(
    "extdata", "example_2", "workflow.R",
    package = "ospsuite.reportingframework",
    mustWork = TRUE
  ))

  return(rootDirectory)
}

#' Build Test Project Directory
#'
#' This function creates a test project directory for users to experiment with examples
#' in the `ospsuite.reportingframework` package.
#'
#' @param rootDirectory A character string specifying the root directory for the test project.
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @examples
#' \dontrun{
#' prepareTestProject("path/to/my/testDirectory")
#' }

#' @export
prepareTestProject <- function(rootDirectory) {

  checkmate::assertDirectory(rootDirectory)

  buildTestData(rootDirectory, verbose, writeTestData = FALSE)
}
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
#' \dontrun{
#'   vignetteSetup <- buildTestProjectory(verbose = TRUE)
#'   # Use vignetteSetup$projectConfiguration and vignetteSetup$scenarioList as needed
#' }
#'
#' @keywords internal
buildTestData  <- function(rootDirectory = NULL,
                           verbose = FALSE,
                           writeTestData){

  synchronizeDirectories <- function(fromDir,toDir,pattern = NULL){

    if (!dir.exists(toDir)) dir.create(toDir)

    filesToCopy = setdiff(
      list.files(path = fromDir,pattern = pattern),
      list.files(path = toDir,pattern = pattern))

    invisible(lapply(filesToCopy,
                     function(f){
                       file.copy(from = file.path(fromDir,f),
                                 to = file.path(toDir,f))
                     }))

    return(invisible())
  }

  instDirectory <- system.file(
    package = "ospsuite.reportingframework",
    "extdata",
    mustWork = TRUE)

  if (is.null(rootDirectory)){
    rootDirectory <- file.path(tempdir(),'testProject')
    if (verbose) message(rootDirectory)
  }
  if (!dir.exists(file.path(rootDirectory, "Scripts", "ReportingFramework"))) {
    dir.create(file.path(rootDirectory, "Scripts", "ReportingFramework"), recursive = TRUE)
  }

  setWorkflowOptions(isValidRun = FALSE)

  # Setup project
  initProject(configurationDirectory = file.path(rootDirectory, "Scripts", "ReportingFramework"))  # Initialize the project structure

  # Get paths of all relevant project files
  projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
    path = file.path(file.path(rootDirectory, "Scripts", "ReportingFramework","ProjectConfiguration.xlsx"))
  )

  initLogfunction(projectConfiguration = projectConfiguration,verbose = verbose)

  # Perform mock manual editings on the project configuration tables
  # Clear existing data from relevant Excel sheets
  mockManualEditings.Cleanup(projectConfiguration)
  # Define virtual populations within biometric ranges
  mockManualEditings.Population(projectConfiguration)
  # Set up scenarios based on the defined populations, add PK Parameter and output paths
  mockManualEditings.Scenario(projectConfiguration)
  ospsuite.reportingframework:::synchronizeScenariosOutputsWithPlots(projectConfiguration)  # Sync outputs with plots
  ospsuite.reportingframework:::synchronizeScenariosWithPlots(projectConfiguration)  # Sync scenarios with plots
  # Define display names for the project
  mockManualEditings.displayNames(projectConfiguration)
  # Add PK parameters to the project configuration
  mockManualEditings.PKParameter(projectConfiguration)


  # Copy files needed for example to the correct folders
  synchronizeDirectories(fromDir = file.path(instDirectory,'Models'),
                         toDir = file.path(projectConfiguration$modelFolder))

  # Recreate results
  synchronizeDirectories(fromDir = file.path(instDirectory,'Populations'),
                         toDir = file.path(projectConfiguration$populationsFolder))


  # Exports all populations defined in the population.xlsx sheet "Demographics"
  suppressMessages(exportRandomPopulations(projectConfiguration = projectConfiguration,
                                           populationNames = NULL,
                                           overwrite = FALSE))

  if (writeTestData){
    if (!dir.exists(file.path(instDirectory,'Populations')))
      dir.create(file.path(instDirectory,'Populations'))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$populationsFolder),
                           toDir = file.path(instDirectory,'Populations'))

  }

  # Simulations
  # Set up Scenarios

  # Initialize all scenarios previously defined in scenario.xlsx
  scenarioList <- createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                          scenarioNames = NULL,
                                          doCheckScenarioNameValidity = TRUE)

  synchronizeDirectories(fromDir = file.path(instDirectory,EXPORTDIR$simulationResult),
                         toDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$simulationResult))

  synchronizeDirectories(fromDir = file.path(instDirectory,EXPORTDIR$pKAnalysisResults),
                         toDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$pKAnalysisResults))

  # Run initialized scenarios and Calculate PK Parameters
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)

  if (writeTestData){
    if (!dir.exists(file.path(instDirectory,EXPORTDIR$simulationResult)))
      dir.create(file.path(instDirectory,EXPORTDIR$simulationResult))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$simulationResult),
                           toDir = file.path(instDirectory,EXPORTDIR$simulationResult))

    if (!dir.exists(file.path(instDirectory,EXPORTDIR$pKAnalysisResults)))
      dir.create(file.path(instDirectory,EXPORTDIR$pKAnalysisResults))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$pKAnalysisResults),
                           toDir = file.path(instDirectory,EXPORTDIR$pKAnalysisResults),
                           pattern = '.csv')
  }

  return(list(projectConfiguration = projectConfiguration,
              scenarioList = scenarioList,
              scenarioResults = scenarioResults))
}




