XXCHUNKstart-librariesXXX
library(ospsuite.reportingframework)
XXCHUNKend-librariesXXX

# set working directory to source file location!

# adjust Path. Please select appropriate directory to set up project structure
projectDirectory <- 'XXprojectDirectoryXX'

# Please decide if you want to do a cleanup, deleting all imported files and
# leaving only the newly generated outputs
cleanUp = TRUE

logCatch(expr = {

  setWorkflowOptions(isValidRun = getQCpassedEnvironmentVariable())

  # build structure needed for workflow and import input files for workflow
  projectConfiguration <- importWorkflow(projectDirectory = projectDirectory,
                                        wfIdentifier = XXwfIdentifierXX,
                                        ePackageFolder = '.',
                                        configurationsFolder = XXconfigurationsFolderXX)
XXCHUNKstart-pathsCustomfunctionsXXX
XXCHUNKend-pathsCustomfunctionsXXX
XXCHUNKstart-dataObservedXXX
XXCHUNKend-dataObservedXXX
XXCHUNKstart-dataObservedPKXXX
XXCHUNKend-dataObservedPKXXX
XXCHUNKstart-scenarioNamesXXX
XXCHUNKend-scenarioNamesXXX
XXCHUNKstart-loadResultsXXX
  # create list of scenarios
  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = c('XXscenarioNamesXX'))

  # run simulations and calculate PK Parameter
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           showProgress = TRUE
                                         ))
XXCHUNKend-loadResultsXXX
XXCHUNKstart-runPlotXXX
XXCHUNKend-runPlotXXX

  # finalize workflow---------------------
  addMessageToLog("finalize workflow")

  # save Session Infos including the loaded packages and R version, into a log file
  saveSessionInfo()
}, finallyExpression = {
  if (cleanUp){
    # Cleanup all temporary files
    for (folder in c(projectConfiguration$populationsFolder,
                     projectConfiguration$modelFolder,
                     projectConfiguration$dataFolder,
                     projectConfiguration$configurationsFolder)){
      if (dir.exists(folder))
        unlink(folder, recursive = TRUE)
    }
  }
})
