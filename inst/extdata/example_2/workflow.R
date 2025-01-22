# Purpose: Test case and base for Tutorial
#

rootDirectory <- '~/TestPK'
message(rootDirectory)
if (!dir.exists(file.path(rootDirectory,'Scripts','ReportingFramework')))
  dir.create(file.path(rootDirectory,'Scripts','ReportingFramework'),recursive = TRUE)


setwd("~/GitHub/OSPSuite.Plots")
load_all()
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

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette OSPSuite_ReportingFramework)
setWorkflowOptions(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
initProject()

# copy files needed for tutorials to the correct folders
file.copy(from = system.file(
  "extdata","example_1","iv 1 mg (5 min).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"iv 1 mg (5 min).pkml"),overwrite = TRUE)

# copy files needed for tutorials to the correct folders
file.copy(from = system.file(
  "extdata","example_1","po 3 mg (solution).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"po 3 mg (solution).pkml"),overwrite = TRUE)


# get paths of all relevant project files
projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

# start log Catch loop which catches all errors, warnins and messages in a logfile
# (see vignette OSPSuite_ReportingFramework)
#logCatch({

  # initialize log file
  initLogfunction(projectConfiguration = projectConfiguration)

  # read observed data
  #dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)


  # export populations ------------------------------------------------------
  mockManualEditings.Population(projectConfiguration)

  # exports all populations defined in population.xlsx sheet "Demographics"
  exportRandomPopulations(projectConfiguration = projectConfiguration,
                          populationNames = NULL,
                          overwrite = FALSE)


  # Simulations ------------------------------------------------------
  # set up Scenarios
  mockManualEditings.Scenario(projectConfiguration)

  # initialize  all scenarios previously defined in scenario.xlsx
  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = NULL,
                            doCheckScenarioNameValidity = TRUE)

  # calculate PK Parameter
  mockManualEditings.PKParameter(projectConfiguration)
  mockManualEditings.outputPath(projectConfiguration)

  # run initialized scenarios
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)


  pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                              scenarioList = scenarioList)


  # Create Output Plots -----------------------------------------------------
  # (see vignette OSPSuite_ReportingFramework.Rmd  section  Plot Functionality)
  # figures and captions are filed in <rootdirectory>\Outputs\ReportingFramework\TimeProfiles

  ## Box whisker different Populations --------
  addDefaultConfigForPKBoxwhsikerPlots(projectConfiguration = projectConfiguration,
                                                   pkParameterDT = pkParameterDT,
                                                   sheetName = "PKParameter_Boxplot",
                                                   overwrite = TRUE)
  mockManualEditings.PlotBoxwhsiker(projectConfiguration)

  runPlot(
    functionKey = "PK_Boxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot",
    inputs = list(
      pkParameterDT = pkParameterDT,
      colorVector = c(pediatric = NA,adult = NA)
    )
  )

  ## Box whisker same Populations --------
  addDefaultConfigForPKBoxwhsikerPlots(projectConfiguration = projectConfiguration,
                                       pkParameterDT = pkParameterDT,
                                       sheetName = "PKParameter_Boxplot2",
                                       overwrite = TRUE)
  mockManualEditings.PlotBoxwhsiker(projectConfiguration)


  runPlot(
    functionKey = "PK_Boxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      colorVector = c(po = NA,iv = NA)
    )
  )

  # Create Report document --------------------------------------------------
  mergeRmds(projectConfiguration = projectConfiguration,
            newName = "appendix",
            title = "Appendix",
            sourceRmds = c("PKParameter_Boxplot",
                           "PKParameter_Boxplot2")
  )

  renderWord(fileName = file.path(projectConfiguration$outputFolder,"appendix.Rmd"))


#})

# finalize workflow---------------------
addMessageToLog("finalize workflow")

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()

