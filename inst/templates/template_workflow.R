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

# all plots will be configured with watermark
setOspsuite.plots.option(
  optionKey = OptionKeys$watermark_enabled,
  value = TRUE
)


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

# get paths of all relevant project files
projectConfiguration <-
  createDefaultProjectConfiguration.wrapped(
    path = file.path(myProjectPath, "ProjectConfiguration.xlsx")
  )


# Read observedData -------------------------------------------------------
# (see vignette('data_import_by_dictionary'))

# read data as dta.table
dataDT <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)

# if you want to create individuals base on this data call
addBiometricsToConfig(observedData = observedData,
                      projectConfiguration = projectConfiguration)

# convert data.table to dataCombined format
dataCombined <- convertDataTableToDataCombined()


# Simulations ------------------------------------------------------
# (see vignette xxx)
scenarioList <-
  createScenarios.wrapped(projectConfiguration = projectConfiguration)

scenarioResults <- runScenarios.wrapped(scenarioList = scenarioList)

saveScenarioResults.wrapped(
  simulatedScenariosResults = resultList,
  projectConfiguration = projectConfiguration
)


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

# TODO
# runPlot(
#   functionKey = "TimeProfile",
#   projectConfg = projectConfiguration,
#   inputs = list(
#     configTable = "TimeProfiles",
#     datacombined = datacombined,
#     observedAsAggregated = TRUE,
#     prepareElectronicPackage = TRUE
#   )
# )

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

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
