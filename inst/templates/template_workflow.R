# Workflow template for ospsuite.reportingframework.
#
# Typical location:
# <RootDirectory>/Scripts/<analysisSubfolderName>
#
# Quick documentation entry points:
# - Workflow vignette: vignette("Workflow", package = "ospsuite.reportingframework")
# - Plot/report vignette: vignette("Plot_and_Report_Generation", package = "ospsuite.reportingframework")
# - Sensitivity vignette: vignette("Sensitivity_Analysis", package = "ospsuite.reportingframework")
# - Function help: ?initProject, ?setWorkflowOptions, ?runPlot, ?mergeQmds

# Initialization -------------------------------------------------------------
# Load package and set plotting/report defaults.
library(ospsuite.reportingframework)

# Set graphic defaults
ggplot2::theme_set(ospsuite.plots::theme_osp())
options(knitr.kable.NA = '')

runPlotTheme <- ggplot2::theme(
  legend.position = "top",
  legend.title = ggplot2::element_blank()
)

# Run mode: see ?setWorkflowOptions.
# If isValidRun is NULL (default), QCpassed environment variable is used.
setWorkflowOptions()

# Setup project structure ----------------------------------------------------
# In interactive RStudio sessions, set wd to this file's folder.
if (
  interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
) {
  activeDocPath <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(activeDocPath)) {
    setwd(dirname(activeDocPath))
  }
}

# Initialize project structure and configuration snapshot.
# See ?initProject for layout options and arguments.
analysisSubfolderName <- basename(getwd())
projectDirectory <- file.path("..", "..")
configurationDirectory <- "."

initProject(
  projectDirectory = projectDirectory,
  configurationDirectory = configurationDirectory,
  analysisSubfolderName = analysisSubfolderName,
  overwrite = TRUE,
  keepEsqlabsRStructure = FALSE
)

# Configuration in Git -------------------------------------------------------
# Keep *.xlsx local. Commit ProjectConfiguration.json (source of truth).

# Load project configuration from ProjectConfiguration.xlsx.
projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx")
)
# Initialize logging
setLogPath(
  logFolder = file.path(projectConfiguration$outputFolder, "logs"),
  logFileName = "workflow.log"
)

# After editing configuration tables, refresh ProjectConfiguration.json.
# snapshotProjectConfigurationRF(
#   projectConfiguration,
#   outputDir = configurationDirectory
# )

# To recreate *.xlsx from JSON (e.g., CI/new machine), use:
# projectConfiguration <- restoreProjectConfigurationRF(
#   snapshotFilePath = file.path(
#     configurationDirectory, "ProjectConfiguration.json"
#   ),
#   outputDir = configurationDirectory
# )

# 1) Read observed data ------------------------------------------------------
# Docs: vignette("Data_import_by_dictionary", package = "ospsuite.reportingframework")
dataObserved <- readObservedDataByDictionary(
  projectConfiguration = projectConfiguration,
  dataClassType = 'timeprofile',
  fileIds = NULL
)

# 2) Export populations ------------------------------------------------------
# Docs: vignette("Population", package = "ospsuite.reportingframework")
# Optional: export random populations.
# exportRandomPopulations(
#   projectConfiguration = projectConfiguration,
#   populationNames = NULL,
#   overwrite = FALSE
# )

# Optional: export virtual twin populations (set modelFile).
# exportVirtualTwinPopulations(
#   projectConfiguration = projectConfiguration,
#   populationNames = NULL,
#   modelFile = "myModelFile.pkml",
#   overwrite = FALSE
# )

# 3) Simulations -------------------------------------------------------------
# Build scenarios, run/load simulations, then calculate PK parameters.
scenarioList <- createScenariosWrapped(
  projectConfiguration = projectConfiguration,
  scenarioNames = NULL
)

# Run or load initialized scenarios and calculate PK Parameters
scenarioResults <- runOrLoadScenarios(
  projectConfiguration = projectConfiguration,
  scenarioList = scenarioList,
  simulationRunOptions = ospsuite::SimulationRunOptions$new(
    numberOfCores = NULL,
    checkForNegativeValues = NULL,
    showProgress = TRUE
  )
)

# Docs: vignette("PK-Parameter", package = "ospsuite.reportingframework")
pkParameterDT <- loadPKParameter(
  projectConfiguration = projectConfiguration,
  scenarioListOrResult = scenarioResults
)

# 4) Sensitivity analysis ----------------------------------------------------
# Docs: vignette("Sensitivity_Analysis", package = "ospsuite.reportingframework")
# Optional: run sensitivity analysis.
# runSensitivityAnalysis(
#   scenario = "MyScenario",
#   configTable = projectConfiguration$SensitivityParameter
# )

# 5) Create output plots -----------------------------------------------------
# Docs: vignette("Plot_and_Report_Generation", package = "ospsuite.reportingframework")
# Example: time-profile plot.
runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
)

# 6) Create report document --------------------------------------------------
# Docs: vignette("Plot_and_Report_Generation", package = "ospsuite.reportingframework")
mergeQmds(
  projectConfiguration = projectConfiguration,
  newName = "appendix",
  title = "Appendix",
  sourceRmds = c(
    "Demographics",
    "TimeProfile",
    "PKParameter",
    "DDIRatio",
    "myFigures"
  )
)

# Render report to Word.
renderWord(
  fileName = file.path(projectConfiguration$outputFolder, "appendix.Rmd")
)

# Finalize workflow ----------------------------------------------------------
addMessageToLog("Finalizing workflow")

# Save session info (package versions, R version) to log.
saveSessionInfo()
