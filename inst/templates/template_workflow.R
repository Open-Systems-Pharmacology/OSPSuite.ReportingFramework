# Purpose: Add the Purpose of the workflow
#

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(ospsuite.reportingframework)
library(ospsuite.plots)
library(ggplot2)
library(esqlabsR)

# set graphic
# (see vignette TODO)
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')

# set options to enable watermarks
# (see vignette TODO)
setOspsuite.plots.option(
  optionKey = OptionKeys$watermark_enabled,
  value = TRUE
)

# Setup project structure -------------------------------------------------
# creates project directory (see vignette TODO Esqlabs)
# and help initProject for source Folder Selection
# if you go with default structure defined by  'sourceFolder = templateDirectory()'
# this workflow file should be saved in scripts/ReportingFramework,
# root directory  is the two layers up.
initProject(
  rootDirectory = file.path("..",'..'),
  sourceFolder = templateDirectory(),
  overwrite = FALSE
)


# get paths of all relevant project files
projectConfiguration <-
  esqlabsR::createDefaultProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))


logCatch({

  # initialize log file for logCatch, has to be first call in logCatch loop
  initLogfunction(projectConfiguration)


  # Read observedData -------------------------------------------------------
  # (see vignette('data_import_by_dictionary'))

  # read data as data.table
  dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                         addBiometricsToConfigFlag = TRUE)


  # Simulations ------------------------------------------------------
  # (see vignette xxx)
  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = NULL,
                            doCheckScenarioNameValidity = TRUE)

  runAndSaveScenarios(projectConfiguration = projectConfiguration,
                      scenarioList = scenarioList,
                      simulationRunOptions = SimulationRunOptions$new(
                        numberOfCores = NULL,
                        checkForNegativeValues = NULL,
                        showProgress = TRUE
                      ))

  # SensitivityAnalysis -----------------------------------------------------
  #  (see vignette xxx)
  # TODO
  # runSensitivityAnalysis(
  #   scenario = "MyScenario",
  #   configTable = projectConfiguration$SensitivityParameter
  # )


  # Create Output Plots -----------------------------------------------------
  # (see vignette xxx)

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
  # see vignette xxx

  runPlot(
    functionKey = "TimeProfile_Panel",
    projectConfiguration = projectConfiguration,
    inputs = list(
      configTableSheet = "TimeProfiles_1",
      dataObserved = dataObserved
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

  #TODO
  # runPlot(
  #   functionKey = NULL,
  #   plotFunction = myProjectSpecificfunction(),
  #   subfolder = "myFigures",
  #   projectConfg = projectConfiguration,
  #   inputs = list()
  # )



  # Create Report document --------------------------------------------------
  #TODO
  # mergeRmds(
  #   newName = "appendix",
  #   title = "Appendix",
  #   sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures")
  # )

  renderWord(fileName = file.path(projectConfiguration$outputFolder,"appendix.rmd"))

  # finalize workflow---------------------
  addMessageToLog("finalize workflow")

})
# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
