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

# initialize Logfile
initLogfunction(projectPath)

# get paths of all relevant project files
projectConfig <- createProjectConfiguration(path = file.path(projectPath, "ProjectConfiguration.xlsx"))


# Simulations ------------------------------------------------------
# (see vignette xxx)
scenarioList <- esqlabsR::createScenarios(
  readScenarioConfigurationFromExcel(
    scenarioNames = "MyScenario",
    projectConfiguration = projectConfig
  )
)

scenarioResults <- esqlabsR::runScenarios(scenarioList)

esqlabsR::saveScenarioResults(scenarioResults,
  projectConfiguration = projectConfig,
  outputFolder = projectConfig$outputFolder
)


# SensitivityAnalysis -----------------------------------------------------
#  (see vignette xxx)
Vision::runSensitivityAnalysis(
  scenario = "MyScenario",
  configTable = projectConfig$SensitivityParameter
)


# Read observedData -------------------------------------------------------
# (see vignette xxx)
datacombined <- Vision::readObservedData(projectConfig = projectConfig)

# Create Output Plots -----------------------------------------------------
# (see vignette xxx)

Vision::runPlot(
  functionKey = "Demographics",
  projectConfg = projectConfig,
  inputs = list(
    configTable = "Demographics",
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

Vision::runPlot(
  functionKey = "TimeProfile",
  projectConfg = projectConfig,
  inputs = list(
    configTable = "TimeProfiles",
    datacombined = datacombined,
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

Vision::runPlot(
  functionKey = "PKParameter",
  projectConfg = projectConfig,
  inputs = list(configTable = "PKParameter")
)

Vision::runPlot(
  functionKey = "DDIRatio",
  projectConfg = projectConfig,
  inputs = list(configTable = "DDIRatio")
)

Vision::runPlot(
  functionKey = NULL,
  plotFunction = myProjectSpecificfunction(),
  subfolder = "myFigures",
  projectConfg = projectConfig,
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

# initialize Logfile
logfilefolder <<- iniLogFile(projectPath)

# get pathes of all relevant project files
projectConfig <-
  esqlabsR::createDefaultProjectConfiguration(path = "./ProjectConfiguration.xlsx")


# Simulations ------------------------------------------------------
# (see vignette xxx)
scenarioList <- esqlabsR::createScenarios(
  readScenarioConfigurationFromExcel(
    scenarioNames = "MyScenario",
    projectConfiguration = projectConfig
  )
)

scenarioResults <- esqlabsR::runScenarios(scenarioList)

esqlabsR::saveScenarioResults(scenarioResults,
  projectConfiguration = projectConfig,
  outputFolder = projectConfig$outputFolder
)


# SensitivityAnalysis -----------------------------------------------------
#  (see vignette xxx)
Vision::runSensitivityAnalysis(
  scenario = "MyScenario",
  configTable = projectConfig$SensitivityParameter
)


# Read observedData -------------------------------------------------------
# (see vignette xxx)
datacombined <- Vision::readObservedData(projectConfig = projectConfig)

# Create Output Plots -----------------------------------------------------
# (see vignette xxx)

Vision::runPlot(
  functionKey = "Demographics",
  projectConfg = projectConfig,
  inputs = list(
    configTable = "Demographics",
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

Vision::runPlot(
  functionKey = "TimeProfile",
  projectConfg = projectConfig,
  inputs = list(
    configTable = "TimeProfiles",
    datacombined = datacombined,
    observedAsAggregated = TRUE,
    prepareElectronicPackage = TRUE
  )
)

Vision::runPlot(
  functionKey = "PKParameter",
  projectConfg = projectConfig,
  inputs = list(configTable = "PKParameter")
)

Vision::runPlot(
  functionKey = "DDIRatio",
  projectConfg = projectConfig,
  inputs = list(configTable = "DDIRatio")
)

Vision::runPlot(
  functionKey = NULL,
  plotFunction = myProjectSpecificfunction(),
  subfolder = "myFigures",
  projectConfg = projectConfig,
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
