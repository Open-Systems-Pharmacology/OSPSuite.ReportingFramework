# Purpose: Test case for PKParameter calculation and plotting
# Editings of the configuration tables which are done typically manually are done with function starting mockManualEditings
#

# Initialization ----------------------------------------------------------
# Load libraries and source project-specific code
library(ospsuite.reportingframework)

# Set graphic defaults for plots
# (See vignette vignette(package = 'ospsuite.plots', topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()  # Set default plotting parameters
theme_update(legend.position = 'top')  # Update theme for legend position

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It will not set watermarks to figures and does not skip failing plot generations
# (See vignette OSPSuite_ReportingFramework)
setWorkflowOptions(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
# creates project directory (see vignette https://esqlabs.github.io/esqlabsR/articles/esqlabsR-project-structure.html)
# and help initProject
# if you go with default structure this workflow file should be saved in Scripts/ReportingFramework,
initProject()  # Initialize the project structure

# Get paths of all relevant project files
projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx")
)

# Setup xlsx configuration
source(system.file(
  "extdata", "example_2", "mockManualEditings.R",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))

# Perform mock manual editings on the project configuration
mockManualEditings.Cleanup(projectConfiguration)  # Clear existing data from relevant Excel sheets
mockManualEditings.DataDictionary(projectConfiguration)  # Update the data dictionary with new data files
mockManualEditings.Population(projectConfiguration)  # Define virtual populations within biometric ranges
mockManualEditings.Scenario(projectConfiguration)  # Set up scenarios based on the defined populations
ospsuite.reportingframework:::synchronizeScenariosOutputsWithPlots(projectConfiguration)  # Sync outputs with plots
ospsuite.reportingframework:::synchronizeScenariosWithPlots(projectConfiguration)  # Sync scenarios with plots
mockManualEditings.outputPath(projectConfiguration)  # Define output paths for the project
mockManualEditings.PKParameter(projectConfiguration)  # Add PK parameters to the project configuration

# Copy files needed for example to the correct folders
file.copy(from = system.file(
  "extdata", "example_1", "iv 1 mg (5 min).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"iv 1 mg (5 min).pkml"), overwrite = TRUE)

file.copy(from = system.file(
  "extdata", "example_1", "po 3 mg (solution).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"po 3 mg (solution).pkml"), overwrite = TRUE)

# Source the script to generate random data
source(system.file(
  "extdata", "example_2", "generateRandomData.R",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))

# Start log catch loop which captures all errors, warnings, and messages in a logfile
# (See vignette OSPSuite_ReportingFramework)
logCatch({

  # Initialize log file for the workflow
  initLogfunction(projectConfiguration = projectConfiguration)

  # Read observed data
  dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                                 dataClassType = 'pkParameter')

  # Export populations ------------------------------------------------------

  # Exports all populations defined in the population.xlsx sheet "Demographics"
  exportRandomPopulations(projectConfiguration = projectConfiguration,
                          populationNames = NULL,
                          overwrite = FALSE)

  # Simulations ------------------------------------------------------
  # Set up Scenarios

  # Initialize all scenarios previously defined in scenario.xlsx
  scenarioList <- createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                          scenarioNames = NULL,
                                          doCheckScenarioNameValidity = TRUE)

  # Calculate PK Parameters

  # Run initialized scenarios
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)

  # Load PK parameter data from the simulations
  pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                   scenarioList = scenarioList)

  # Create Output Plots -----------------------------------------------------
  # (See vignette OSPSuite_ReportingFramework.Rmd section Plot Functionality)
  # Figures and captions are filed in <rootdirectory>\Outputs\ReportingFramework\TimeProfiles

  ## Case 1: Different Populations --------
  ### Box-whisker Plot --------
  addDefaultConfigForPKBoxwhsikerPlots(projectConfiguration = projectConfiguration,
                                       pkParameterDT = pkParameterDT,
                                       sheetName = "PKParameter_Boxplot",
                                       overwrite = TRUE)
  mockManualEditings.PlotBoxwhsiker1(projectConfiguration)

  # Run the box-whisker plot function
  runPlot(
    nameOfplotFunction = "plotPKBoxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot1",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  ### Forest Plot --------
  addDefaultConfigForPKForestPlots(projectConfiguration = projectConfiguration,
                                   pkParameterDT = pkParameterDT,
                                   sheetName = "PKParameter_ForestAbs1",
                                   overwrite = TRUE)

  mockManualEditings.PlotForest1(projectConfiguration, sheetName = "PKParameter_ForestAbs1")

  # Run the forest plot for aggregated absolute values
  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbs1",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  # Run the forest plot for point estimates of absolute values
  mockManualEditings.PlotForest1(projectConfiguration, sheetName = "PKParameter_ForestAbsPE1")

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbsPE1",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  # Run the forest plot for point estimates of ratios
  mockManualEditings.PlotForest1(projectConfiguration, sheetName = "PKParameter_ForestRatioPE1");

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatioPE1",
    inputs = list(
      pkParameterDT = pkParameterDT,
      pkParameterObserved = dataObservedPK
    )
  )

  ## Case 2: Same Populations --------
  ### Box-whisker Plot for Same Populations --------
  addDefaultConfigForPKBoxwhsikerPlots(projectConfiguration = projectConfiguration,
                                       pkParameterDT = pkParameterDT,
                                       sheetName = "PKParameter_Boxplot2",
                                       overwrite = TRUE)
  mockManualEditings.PlotBoxwhsiker2(projectConfiguration)

  # Run the box-whisker plot function for same populations
  runPlot(
    nameOfplotFunction = "plotPKBoxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot2",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  ## Forest Plot for Same Populations --------
  addDefaultConfigForPKForestPlots(projectConfiguration = projectConfiguration,
                                   pkParameterDT = pkParameterDT,
                                   sheetName = "PKParameter_ForestAbs2",
                                   overwrite = TRUE)

  mockManualEditings.PlotForest2(projectConfiguration, sheetName = "PKParameter_ForestAbs2")

  # Run the forest plot for aggregated absolute values for same populations
  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbs2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      aggregationFlag = "Percentiles",
      percentiles = c(0.025, 0.5, 0.975)
    )
  )

  # Run the forest plot for point estimates of absolute values
  mockManualEditings.PlotForest2(projectConfiguration, sheetName = "PKParameter_ForestRatio2")

  customAggregationFunction <-
    function(y) {
      y <- y[!is.na(y)]
      return(list(
        yMin = stats::quantile(y, probs = 0.25),
        yValues = stats::quantile(y, probs = 0.5),
        yMax = stats::quantile(y, probs = 0.75),
        yErrorType = 'Median | Q1 | Q3'
      ))
    }

  # Run the forest plot for aggregated ratios with a custom aggregation function
  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatio2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      aggregationFlag = "Custom",
      customFunction = customAggregationFunction
    )
  )

  # Run the forest plot for point estimates of absolute values
  mockManualEditings.PlotForest2(projectConfiguration, sheetName = "PKParameter_ForestAbsPE2")

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbsPE2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      nObservationDefault = 16
    )
  )

  # Run the forest plot for point estimates of ratios
  mockManualEditings.PlotForest2(projectConfiguration, sheetName = "PKParameter_ForestRatioPE2")

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatioPE2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      pkParameterObserved = dataObservedPK
    )
  )

  # Create Report Document --------------------------------------------------
  mergeRmds(projectConfiguration = projectConfiguration,
            newName = "appendix",
            title = "Appendix",
            sourceRmds = c("PKParameter_Boxplot1",
                           "PKParameter_ForestAbs1",
                           "PKParameter_ForestAbsPE1",
                           "PKParameter_ForestRatioPE1",
                           "PKParameter_Boxplot2",
                           "PKParameter_ForestAbs2",
                           "PKParameter_ForestRatio2",
                           "PKParameter_ForestAbsPE2",
                           "PKParameter_ForestRatioPE2"
            )
  )

  # Render the report to a Word document
  renderWord(fileName = file.path(projectConfiguration$outputFolder, "appendix.Rmd"))

})

# Finalize workflow---------------------
addMessageToLog("Finalize workflow")

# Save session info including the loaded packages and R version, into a log file
saveSessionInfo()
