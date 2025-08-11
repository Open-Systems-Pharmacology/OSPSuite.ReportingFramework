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
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')
options(knitr.kable.NA = '')

# Set this to TRUE if you want to execute the workflow as a final valid run.
# (see ?setWorkflowOptions)
setWorkflowOptions(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
# Create project directory and initialize the structure
# (see ?initProject and https://esqlabs.github.io/esqlabsR/articles/esqlabsR.html)
initProject()

# Get paths of all relevant project files and folders
projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx")
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
scenarioList <- createScenarios.wrapped(
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
# Add your different plot functions below; the following is an exemplary call for the time profile plot function
runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  inputs = list(
    configTableSheet = "TimeProfiles",
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
  sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures")
)

# Render the report to Word format
renderWord(fileName = file.path(projectConfiguration$outputFolder, "appendix.Rmd"))

# Finalize Workflow -----------------------------------------------------
addMessageToLog("Finalizing workflow")

# Save session information including loaded packages and R version into a log file
saveSessionInfo()
