# Purpose: Add the Purpose of the workflow
#

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(ospsuite.reportingframework)


# set graphic
# (see vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')
# configure panel labels to be used as Tags A,B,...
theme_update(strip.background = element_rect(fill = NA,color = NA))
theme_update(strip.text = element_text(hjust = 0,vjust = 1))

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette TODO)
executeAsValidRun(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
# creates project directory
# (see help initProject and https://esqlabs.github.io/esqlabsR/articles/esqlabsR.html)
# if you go with the default structure defined by 'sourceFolder = templateDirectory()'
# this workflow file should be saved in Scripts/ReportingFramework,
# root directory is then two layers up.
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
  # (see vignette(package = 'ospsuite.reportingframework',topic = 'data_import_by_dictionary'))

  # read data as data.table
  dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                         addBiometricsToConfigFlag = TRUE)


  # Simulations ------------------------------------------------------
  # (see https://esqlabs.github.io/esqlabsR/articles/esqlabsR.html)
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
  # see vignette(package = 'ospsuite.reportingframework',topic = 'TimeProfilePlots')

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
