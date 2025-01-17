# Purpose: Add the Purpose of the workflow
#

# Typically This file is saved in <Rootdirectory>\Scripts\ReportingFramework

# set working directory to  workflow file location, (only if working interactively)
if (interactive() && rstudioapi::isAvailable()) {
  # Get the active document path in RStudio
  activeDocPath <- rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(activeDocPath))
}


# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(ospsuite.reportingframework)

# set graphic
# (see vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette OSPSuite_ReportingFramework)
setWorkflowOptions(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
# creates project directory
# (see help initProject and https://esqlabs.github.io/esqlabsR/articles/esqlabsR.html)
# if you go with the default structure
# this workflow file should be saved in Scripts/ReportingFramework,
initProject()


# get paths of all relevant project files
projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

# start log Catch loop which catches all errors, warnins and messages in a logfile
# (see vignette OSPSuite_ReportingFramework)
logCatch({

  # initialize log file for logCatch, has to be first call in logCatch loop
  initLogfunction(projectConfiguration)


  # 1) Read observedData -------------------------------------------------------
  # (see vignette(package = 'ospsuite.reportingframework',topic = 'Data_import_by_dictionary'))

  # read data as data.table
  dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)

  # 2) Export populations -------------------------------------------------------
  # (see vignette Simulation_setup)

  # to export random populations de-comment lines below
  # exportRandomPopulations(
  #   projectConfiguration = projectConfiguration,
  #   populationNames = NULL,
  #   overwrite = FALSE
  # )
  #

  # to export virtual twin populations de-comment lines below and adjsut name of modelfile
  # exportVirtualTwinPopulations(
  #   projectConfiguration = projectConfiguration,
  #   populationNames = NULL,
  #   modelFile = "myModelFile.pkml",
  #   overwrite = FALSE
  # )


  # 3) Simulations ------------------------------------------------------
  # (see  vignette Simulation_setup)
  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = NULL,
                            doCheckScenarioNameValidity = TRUE)

  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           numberOfCores = NULL,
                                           checkForNegativeValues = NULL,
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)

  # 4) SensitivityAnalysis -----------------------------------------------------
  #  (see vignette xxx)
  # TODO
  # runSensitivityAnalysis(
  #   scenario = "MyScenario",
  #   configTable = projectConfiguration$SensitivityParameter
  # )


  # 5) Create Output Plots -----------------------------------------------------
  # (see vignette OSPSuite_ReportingFramework.Rmd  section  Plot Functionality)

  # TODO
  # runPlot(
  #   functionKey = "Demographics",
  #   projectConfg = projectConfiguration,
  #   inputs = list(
  #     configTable = "Demographics",
  #     observedAsAggregated = TRUE,
  #     prepareElectronicPackage = TRUE
  #   )
  # )

  # Timeprofile Plots
  #' The `TimeProfile_Panel` function creates a series of time profile plots as facet panels
  #' from the provided observed data. This function is designed to visualize complex time-dependent
  #' data by allowing for multiple plots organized in a user-defined layout.
  #'
  #' This function is particularly useful for researchers and analysts who need to visualize
  #' concentration-time profiles of various scenarios, compare predicted versus observed data,
  #' and analyze residuals cohesively. For more detailed information, please refer to the
  #' accompanying vignettes:
  #' - **TimeProfilePlots**
  #' - **Time Profile Plotting Tutorial**

  runPlot(
    functionKey = "TimeProfiles",
    projectConfiguration = projectConfiguration,
    inputs = list(
      configTableSheet = "TimeProfiles",
      dataObserved = dataObserved,
      scenarioResults = scenarioResults
    )
  )

  #TODO
  # runPlot(
  #   functionKey = "PKParameter",
  #   projectConfg = projectConfiguration,
  #   inputs = list(configTable = "PKParameter")
  # )


  #TODO
  # runPlot(
  #   functionKey = "DDIRatio",
  #   projectConfg = projectConfiguration,
  #   inputs = list(configTable = "DDIRatio")
  # )

  # see vignette (TODO)
  runPlot(
    functionKey = NULL,
    plotFunction = myProjectSpecificfunction(),
    subfolder = "myCustomPlots",
    projectConfiguration = projectConfiguration,
    inputs = list()
  )

  # 6) Create Report document --------------------------------------------------
  mergeRmds(projectConfiguration = projectConfiguration,
    newName = "appendix",
    title = "Appendix",
    sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures")
  )

  renderWord(fileName = file.path(projectConfiguration$outputFolder,"appendix.Rmd"))

  # finalize workflow---------------------
  addMessageToLog("finalize workflow")

})
# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
