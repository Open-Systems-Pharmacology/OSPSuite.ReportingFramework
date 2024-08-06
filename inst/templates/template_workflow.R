# Purpose: Add the Purpose of the workflow
#

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(ospsuite.reportingframework)
library(ospsuite.plots)
library(esqlabsR)

# set graphic all defaults
# (see vignette TODO)
ospsuite.plots::setDefaults()
theme_update(panel.background = element_rect(linetype = 'solid'))

# set options to switch between non-Valid and valid mode
# (see vignette TODO)
setRunModeIsValid(FALSE)

# Setup project structure -------------------------------------------------
# creates project directory (see vignette TODO Esqlabs)
# and help initProject for source Folder Selection
projectPath <- initProject(
  projectPath = ".",
  overwrite = FALSE,
  sourceFolder = templateDirectory()
)

# initialize log file
initLogfunction(projectPath)

logCatch({

  # get paths of all relevant project files
  projectConfiguration <-
    createDefaultProjectConfiguration.wrapped(
      path = file.path(projectPath, "ProjectConfiguration.xlsx")
    )

  # Read observedData -------------------------------------------------------
  # (see vignette('data_import_by_dictionary'))

  # read data as data.table
  dataDT <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                         addBiometricsToConfigFlag = TRUE)

  # convert data.table to dataCombined format
  # edit DataGroupID in the data Configuration to get proper names
  # edit OutputpathID in the scenario Configuration to get molweights
  dataCombined <-
    convertDataTableToDataCombined(projectConfiguration = projectConfiguration,
                                   dataDT = dataDT)


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
    functionKey = "TimeProfile",
    projectConfg = projectConfiguration,
    inputs = list(
      configTable = "TimeProfiles",
      dataCombined = dataCombined,
      prepareElectronicPackage = TRUE
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
