# Purpose: Add the Purpose of the workflow
#

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(OSPSuite.ReportingFramework)
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
projectConfiguration <- createProjectConfiguration(path = file.path(projectPath, "ProjectConfiguration.xlsx"))


# Simulations ------------------------------------------------------
# (see vignette xxx)
scenarioList <- esqlabsR::createScenarios(
  readScenarioConfigurationFromExcel(
    scenarioNames = "MyScenario",
    projectConfiguration = projectConfiguration
  )
)

scenarioResults <- esqlabsR::runScenarios(scenarioList)

esqlabsR::saveScenarioResults(scenarioResults,
  projectConfiguration = projectConfiguration,
  outputFolder = projectConfiguration$outputFolder
)


# SensitivityAnalysis -----------------------------------------------------
#  (see vignette xxx)
Vision::runSensitivityAnalysis(
  scenario = "MyScenario",
  configTable = projectConfiguration$SensitivityParameter
)


# Read observedData -------------------------------------------------------
# (see vignette xxx)
datacombined <- Vision::readObservedData(projectConfiguration = projectConfiguration)

# Create Output Plots -----------------------------------------------------
# (see vignette xxx)

Vision::runPlot(
  functionKey = "Demographics",
  projectConfg = projectConfiguration,
  inputs = list(
    configTable = "Demographics",
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

Vision::runPlot(
  functionKey = "TimeProfile",
  projectConfg = projectConfiguration,
  inputs = list(
    configTable = "TimeProfiles",
    datacombined = datacombined,
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

Vision::runPlot(
  functionKey = "PKParameter",
  projectConfg = projectConfiguration,
  inputs = list(configTable = "PKParameter")
)

Vision::runPlot(
  functionKey = "DDIRatio",
  projectConfg = projectConfiguration,
  inputs = list(configTable = "DDIRatio")
)

Vision::runPlot(
  functionKey = NULL,
  plotFunction = myProjectSpecificfunction(),
  subfolder = "myFigures",
  projectConfg = projectConfiguration,
  inputs = list()
)



# Create Report document --------------------------------------------------
Vision::mergeRmds(
  newName = "appendix",
  title = "Appendix",
  sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures")
)

vision::renderReport(rmd = "appendix")

# finalise workflow---------------------
addMessageToLog("finalise workflow")

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
# Purpose: Add the Purpose of the workflow
#

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
library(OSPSuite.ReportingFramework)
library(ospsuite.plots)
library(esqlabsR)

# set graphic all defaults
# (see vignette TODO)
ospsuite.plots::setDefaults()

# all plots will be configured with watermark
ospsuite.plots::setOption(watermark.enabled = TRUE)


# Setup project structure -------------------------------------------------
# creates project directory (see vignette TODO Esqlabs)
# and help initProject for source Folder Selection
projectPath <- initProject(
  destination = ".",
  overwrite = FALSE,
  sourceFolder = templateDirectory()
)

# initialize log file
logfilefolder <<- iniLogFile(projectPath)

# get paths of all relevant project files
projectConfiguration <-
  esqlabsR::createDefaultProjectConfiguration(path = "./ProjectConfiguration.xlsx")


# Simulations ------------------------------------------------------
# (see vignette xxx)
scenarioList <- esqlabsR::createScenarios(
  readScenarioConfigurationFromExcel(
    scenarioNames = "MyScenario",
    projectConfiguration = projectConfiguration
  )
)

scenarioResults <- esqlabsR::runScenarios(scenarioList)

esqlabsR::saveScenarioResults(scenarioResults,
  projectConfiguration = projectConfiguration,
  outputFolder = projectConfiguration$outputFolder
)


# # SensitivityAnalysis -----------------------------------------------------
# #  (see vignette xxx)
# Vision::runSensitivityAnalysis(
#   scenario = "MyScenario",
#   configTable = projectConfiguration$SensitivityParameter
# )


# Read observedData -------------------------------------------------------
# (see vignette xxx)
datacombined <- readObservedData(projectConfiguration = projectConfiguration)

# Create Output Plots -----------------------------------------------------
# (see vignette xxx)

runPlot(
  functionKey = "Demographics",
  projectConfg = projectConfiguration,
  inputs = list(
    configTable = "Demographics",
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

runPlot(
  functionKey = "TimeProfile",
  projectConfg = projectConfiguration,
  inputs = list(
    configTable = "TimeProfiles",
    datacombined = datacombined,
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

runPlot(
  functionKey = "PKParameter",
  projectConfg = projectConfiguration,
  inputs = list(configTable = "PKParameter")
)

runPlot(
  functionKey = "DDIRatio",
  projectConfg = projectConfiguration,
  inputs = list(configTable = "DDIRatio")
)

runPlot(
  functionKey = NULL,
  plotFunction = myProjectSpecificfunction(),
  subfolder = "myFigures",
  projectConfg = projectConfiguration,
  inputs = list()
)



# Create Report document --------------------------------------------------
mergeRmds(
  newName = "appendix",
  title = "Appendix",
  sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures")
)

renderReport(rmd = "appendix")

# finalize workflow---------------------
addMessageToLog("finalize workflow")

# save Session infos including the loaded packages and R version, into session.log
saveSessionInfo()
