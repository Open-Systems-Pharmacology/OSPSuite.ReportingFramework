# Purpose: This script serves as a user guide for the ospsuite.reportingframework package,
# providing examples of the various tasks that can be performed.
# It outlines the recommended order of execution for function calls,
# enabling users to effectively process pharmacokinetic data, export populations,
# run simulations, conduct sensitivity analysis, and generate output plots and reports.

# Typically, this file is saved in <Rootdirectory>/Scripts/ReportingFramework

# Initialization  ----------------------------------------------------------
# Load necessary libraries and source project-specific code
library(ospsuite.reportingframework)

# Set graphic defaults
# (see vignette(package = 'ospsuite.plots', topic = 'ospsuite_plots'))
ggplot2::theme_set(theme_osp())
theme_update(legend.position = 'top')
options(knitr.kable.NA = '')

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
# This script should be run from the project root directory.
# If running from this script's location, use: setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# and then setwd("../../..")  or use here::here() for reliable path handling.

# Initialize the project (creates base structure and JSON snapshot)
# When called from project root, defaults will place ProjectConfiguration.xlsx in Scripts/ReportingFramework
# A JSON snapshot of the configuration is automatically created for version control
initProject()

# Get paths of all relevant project files and folders
# The ProjectConfiguration.xlsx is now at Scripts/ReportingFramework/ProjectConfiguration.xlsx
projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path("Scripts", "ReportingFramework", "ProjectConfiguration.xlsx")
)

# Initialize log file
initLogfunction(projectConfiguration)

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
# Add your different plot functions below; the following is an exemplary call for the time profile plot function.
# YOu can use addDefaultConfigForTimeProfilePlots to start the configuration table
runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
)

# 6) Create Report Document --------------------------------------------------
# (see vignette(package = 'ospsuite.reportingframework', topic = 'Plot_and_Report_Generation'))
# Adjust the input variables as necessary
mergeRmds(
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
