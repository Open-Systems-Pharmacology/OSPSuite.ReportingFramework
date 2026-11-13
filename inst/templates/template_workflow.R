# Purpose: This script serves as a user guide for the ospsuite.reportingframework package,
# providing examples of the various tasks that can be performed.
# It outlines the recommended order of execution for function calls,
# enabling users to effectively process pharmacokinetic data, export populations,
# run simulations, conduct sensitivity analysis, and generate output plots and reports.

# Typically, this file is saved in
# <RootDirectory>/Scripts/<analysisSubfolderName>

# Initialization  ----------------------------------------------------------
# Load necessary libraries and source project-specific code
library(ospsuite.reportingframework)

# Set graphic defaults
# (see vignette(package = 'ospsuite.plots', topic = 'ospsuite_plots'))
ggplot2::theme_set(theme_osp())
options(knitr.kable.NA = '')

runPlotTheme <- ggplot2::theme(
  legend.position = "top",
  legend.title = ggplot2::element_blank()
)

# Set this to TRUE if you want to execute the workflow as a final valid run.
# (see ?setWorkflowOptions)
# If isValidRun is NULL (default), setWorkflowOptions() derives the value
# from the QCpassed environment variable.
# Example:
#   Sys.setenv(QCpassed = "TRUE")  # valid run
#   Sys.setenv(QCpassed = "FALSE") # exploratory run
setWorkflowOptions()

# Setup project structure -------------------------------------------------
# Create project directory and initialize the structure
# This workflow is expected to be saved in Scripts/<analysisSubfolderName>.
# In interactive RStudio sessions, set the working directory to the folder
# containing this workflow file.
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

# Initialize the project (creates base structure and JSON snapshot)
# Derive analysisSubfolderName from the current working directory.
# The project directory is two levels up from Scripts/<analysisSubfolderName>.
# Set keepEsqlabsRStructure = TRUE if you want to preserve the original
# esqlabsR layout instead of switching to the ReportingFramework default layout.
analysisSubfolderName <- basename(getwd())
projectDirectory <- file.path("..", "..")
configurationDirectory <- "."
if (!identical(basename(dirname(getwd())), "Scripts")) {
  warning(
    "The workflow should run from Scripts/<analysisSubfolderName>. ",
    "Current directory is: ",
    getwd()
  )
}
initProject(
  projectDirectory = projectDirectory,
  configurationDirectory = configurationDirectory,
  analysisSubfolderName = analysisSubfolderName,
  overwrite = TRUE,
  keepEsqlabsRStructure = FALSE
)

# Configuration in Git ------------------------------------------------------
# *.xlsx configuration files are local working files and should not be versioned.
# In configuration folders, initProject() creates or updates .gitignore to
# ignore *.xlsx.
# Commit ProjectConfiguration.json to Git and use it as the source of truth.

# Get paths of all relevant project files and folders
# ProjectConfiguration.xlsx is in the current configuration directory.
projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx")
)
# Initialize logging
setLogPath(
  logFolder = file.path(projectConfiguration$outputFolder, "logs"),
  logFileName = "workflow.log"
)

# After editing configuration tables, refresh the JSON snapshot.
# Commit Scripts/<analysisSubfolderName>/ProjectConfiguration.json.
# snapshotProjectConfigurationRF(
#   projectConfiguration,
#   outputDir = configurationDirectory
# )

# To recreate configuration from JSON (e.g., CI/new machine), use:
# projectConfiguration <- restoreProjectConfigurationRF(
#   snapshotFilePath = file.path(
#     configurationDirectory, "ProjectConfiguration.json"
#   ),
#   outputDir = configurationDirectory
# )

# 1) Read Observed Data -------------------------------------------------------
# Read observed data as data.table
# (see vignette(package = 'ospsuite.reportingframework', topic = 'Data_import_by_dictionary'))
dataObserved <- readObservedDataByDictionary(
  projectConfiguration = projectConfiguration,
  dataClassType = 'timeprofile',
  fileIds = NULL
)

# 2) Export Populations -------------------------------------------------------
# (see vignette(package = 'ospsuite.reportingframework', topic = 'Population'))
# To export random populations, uncomment the lines below
# exportRandomPopulations(
#   projectConfiguration = projectConfiguration,
#   populationNames = NULL,
#   overwrite = FALSE
# )

# To export virtual twin populations, uncomment the lines below and adjust the name of the model file
# exportVirtualTwinPopulations(
#   projectConfiguration = projectConfiguration,
#   populationNames = NULL,
#   modelFile = "myModelFile.pkml",
#   overwrite = FALSE
# )

# 3) Simulations ------------------------------------------------------
# Set up the scenario list
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

# Load PK Parameters
# (see vignette(package = 'ospsuite.reportingframework', topic = 'PK-Parameter'))
pkParameterDT <- loadPKParameter(
  projectConfiguration = projectConfiguration,
  scenarioListOrResult = scenarioResults
)

# 4) Sensitivity Analysis -----------------------------------------------------
# To run a sensitivity analysis, uncomment the following lines and adjust the input variables
# runSensitivityAnalysis(
#   scenario = "MyScenario",
#   configTable = projectConfiguration$SensitivityParameter
# )

# 5) Create Output Plots -----------------------------------------------------
# (see vignette(package = 'ospsuite.reportingframework', topic = 'Plot_and_Report_Generation'))
# Add your plot function calls below; the following is an example for
# time-profile plots. You can use addDefaultConfigForTimeProfilePlots()
# to initialize the corresponding configuration table.
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

# 6) Create Report Document --------------------------------------------------
# (see vignette(package = 'ospsuite.reportingframework', topic = 'Plot_and_Report_Generation'))
# Adjust the input variables as necessary
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

# Render the report to Word format
renderWord(
  fileName = file.path(projectConfiguration$outputFolder, "appendix.Rmd")
)

# Finalize Workflow -----------------------------------------------------
addMessageToLog("Finalizing workflow")

# Save session information including loaded packages and R version into a log file
saveSessionInfo()
