# Purpose: Add the Purpose of the workflow
#

# Typically This file is saved in <Rootdirectory>\Scripts\ReportingFramework

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(ospsuite.reportingframework)

# set graphic
# (see vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')
options(knitr.kable.NA = '')

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette OSPSuite_ReportingFramework)
setWorkflowOptions(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
# creates project directory
# (see help initProject and https://esqlabs.github.io/esqlabsR/articles/esqlabsR.html)
# if you go with the default structure
# this workflow file should be saved in <Rootdirectory>/Scripts/ReportingFramework,
initProject()


# get paths of all relevant project files
projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

# start log Catch loop which catches all errors, warnins and messages in a logfile
# (see vignette OSPSuite_ReportingFramework)

# initialize log file for logCatch, has to be first call in logCatch loop
initLogfunction(projectConfiguration)


# 1) Read observedData -------------------------------------------------------
# (see vignette(package = 'ospsuite.reportingframework',topic = 'Data_import_by_dictionary'))

# read data as data.table
dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                             dataClassType = 'timeprofile',
                                             fileIds = NULL)

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
                          scenarioNames = NULL)

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

# Timeprofile Plots
#' The `plotTimeProfiles` function creates a series of time profile plots as facet panels
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
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  inputs = list(
    configTableSheet = "TimeProfiles",
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
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


# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
