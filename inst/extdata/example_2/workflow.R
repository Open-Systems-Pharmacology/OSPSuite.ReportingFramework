# Purpose: Test case and base for Tutorial
#

# use example_1 as base
rootDirectory <- '~/Test'
message(rootDirectory)

setwd("~/GitHub/OSPSuiteReportingFramework")
load_all()
setwd(file.path(rootDirectory,'Scripts','ReportingFramework'))


source(system.file(
  "extdata", "example_2", "mockManualEditings.R",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))


# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
#library(ospsuite.reportingframework)

# set graphic all defaults
# (see vignette vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')
# configure panel labels to be used as Tags A,B,...
theme_update(strip.background = element_rect(fill = NA,color = NA))
theme_update(strip.text = element_text(hjust = 0,vjust = 1))

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette OSPSuite_ReportingFramework)
executeAsValidRun(isValidRun = FALSE)
options(OSPSuite.RF.skipFailingPlots = FALSE)

# Setup project structure -------------------------------------------------
# creates project directory (see vignette https://esqlabs.github.io/esqlabsR/articles/esqlabsR-project-structure.html)
# and help initProject for source Folder Selection
# if you go with default structure defined by  'sourceFolder = templateDirectory()'
# this workflow file should be saved in Scripts/ReportingFramework,
# root directory is then two layers up.
initProject(
  rootDirectory = file.path("..",'..'),
  sourceFolder = templateDirectory(),
  overwrite = FALSE
)


# get paths of all relevant project files
projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

# start log Catch loop which catches all errors, warnins and messages in a logfile
# (see vignette OSPSuite_ReportingFramework)
logCatch({

  # initialize log file
  initLogfunction(projectConfiguration = projectConfiguration)

  # read observed data
  dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)


  # Simulations ------------------------------------------------------
  # set up Scenarios


  # initialize  all scenarios previously defined in scenario.xlsx
  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = NULL,
                            doCheckScenarioNameValidity = TRUE)
  # run initialized scenarios
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           numberOfCores = NULL,
                                           checkForNegativeValues = NULL,
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)

  # calculate PK Parameter
  mockManualEditings.PKParameter(projectConfiguration)

  pkParameterDT <- calculateAndLoadPKParameter(projectConfiguration = projectConfiguration,
                                               scenarioResults = scenarioResults,
                                               pkParameterSheets = c('PK_Plasma','PK_Fraction'),
                                               withRecalculation = TRUE)


  # Create Output Plots -----------------------------------------------------
  # (see vignette OSPSuite_ReportingFramework.Rmd  section  Plot Functionality)


  # figures and captions are filed in <rootdirectory>\Outputs\ReportingFramework\TimeProfiles


  # Create Report document --------------------------------------------------
  mergeRmds(projectConfiguration = projectConfiguration,
            newName = "appendix",
            title = "Appendix",
            sourceRmds = c("TimeProfiles")
  )

  renderWord(fileName = file.path(projectConfiguration$outputFolder,"appendix.Rmd"))


})

# finalize workflow---------------------
addMessageToLog("finalize workflow")

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()

