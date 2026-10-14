XXCHUNKstart - librariesXXX
library(ospsuite.reportingframework)
XXCHUNKend - librariesXXX

# Set the working directory to the source file location before running this script.

# Adjust this path to the directory where the workflow-specific project structure should be created.
projectDirectory <- 'XXprojectDirectoryXX'

# Set cleanUp to TRUE to remove imported temporary files and keep only the
# newly generated outputs.
cleanUp <- TRUE

setLogPath(
  logFolder = file.path(projectDirectory, 'logs'),
  logFileName = paste0('w', XXwfIdentifierXX, '.log')
)

ospsuite.reportingframework::captureLog(
  expr = {
    setWorkflowOptions(isValidRun = getQCpassedEnvironmentVariable())

    # Import the workflow input files and create the workflow-specific project structure.
    projectConfiguration <- importWorkflow(
      projectDirectory = projectDirectory,
      wfIdentifier = XXwfIdentifierXX,
      ePackageFolder = '.'
    )
    XXCHUNKstart - pathsCustomfunctionsXXX
    XXCHUNKend - pathsCustomfunctionsXXX
    XXCHUNKstart - dataObservedXXX
    XXCHUNKend - dataObservedXXX
    XXCHUNKstart - dataObservedPKXXX
    XXCHUNKend - dataObservedPKXXX
    XXCHUNKstart - scenarioNamesXXX
    XXCHUNKend - scenarioNamesXXX
    XXCHUNKstart - loadResultsXXX
    # create list of scenarios
    scenarioList <-
      createScenarios.wrapped(
        projectConfiguration = projectConfiguration,
        scenarioNames = c('XXscenarioNamesXX')
      )

    # Run simulations and calculate PK parameters.
    scenarioResults <- runAndSaveScenarios(
      projectConfiguration = projectConfiguration,
      scenarioList = scenarioList,
      simulationRunOptions = ospsuite::SimulationRunOptions$new(
        showProgress = TRUE
      )
    )
    XXCHUNKend - loadResultsXXX
    XXCHUNKstart - runPlotXXX
    XXCHUNKend - runPlotXXX

    # Finalize workflow.
    addMessageToLog("finalize workflow")

    # Save session information, including loaded packages and the R version, to a log file.
    saveSessionInfo(
      fileName = paste0('w', XXwfIdentifierXX, '_sessionInfo.log')
    )
  },
  finallyExpression = {
    if (cleanUp) {
      # Clean up all temporary files.
      for (folder in c(
        projectConfiguration$populationsFolder,
        projectConfiguration$modelFolder,
        projectConfiguration$configurationsFolder,
        projectConfiguration$addOns$dataFileFolder
      )) {
        if (dir.exists(folder)) {
          unlink(folder, recursive = TRUE)
        }
      }
    }
  }
)
