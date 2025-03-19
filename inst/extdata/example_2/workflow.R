# Purpose: Test case and base for Tutorial
#

rootDirectory <- '~/TestPK'
message(rootDirectory)
if (!dir.exists(file.path(rootDirectory,'Scripts','ReportingFramework')))
  dir.create(file.path(rootDirectory,'Scripts','ReportingFramework'),recursive = TRUE)


setwd("~/GitHub/OSPSuite.Plots")
load_all()
setwd("~/GitHub/OSPSuiteReportingFramework")
load_all()
setwd(file.path(rootDirectory,'Scripts','ReportingFramework'))

# Initialization  ----------------------------------------------------------
# load libraries and source project specific code
#library(ospsuite.reportingframework)

##truncStart_init

# set graphic all defaults
# (see vignette vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette OSPSuite_ReportingFramework)
setWorkflowOptions(isValidRun = FALSE)

# Setup project structure -------------------------------------------------
initProject()

# get paths of all relevant project files
projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

##truncStop_init

# setup xlsx
source(system.file(
  "extdata", "example_2", "mockManualEditings.R",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))

mockManualEditings.Cleanup(projectConfiguration)
mockManualEditings.DataDictionary(projectConfiguration)
mockManualEditings.Population(projectConfiguration)
mockManualEditings.Scenario(projectConfiguration)
ospsuite.reportingframework:::synchronizeScenariosOutputsWithPlots(projectConfiguration)
ospsuite.reportingframework:::synchronizeScenariosWithPlots(projectConfiguration)
mockManualEditings.outputPath(projectConfiguration)
mockManualEditings.PKParameter(projectConfiguration)

##truncStart_exampleSetup

# copy files needed for tutorials to the correct folders
file.copy(from = system.file(
  "extdata","example_1","iv 1 mg (5 min).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"iv 1 mg (5 min).pkml"),overwrite = TRUE)

# copy files needed for tutorials to the correct folders
file.copy(from = system.file(
  "extdata","example_1","po 3 mg (solution).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"po 3 mg (solution).pkml"),overwrite = TRUE)

source(system.file(
  "extdata", "example_2", "generateRandomData.R",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))

##truncStop_exampleSetup



# start log Catch loop which catches all errors, warnins and messages in a logfile
# (see vignette OSPSuite_ReportingFramework)
#logCatch({

  # initialize log file
  initLogfunction(projectConfiguration = projectConfiguration)


  # read observed data
  dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                                 dataClassType = 'pkParameter')


  # export populations ------------------------------------------------------

  # exports all populations defined in population.xlsx sheet "Demographics"
  exportRandomPopulations(projectConfiguration = projectConfiguration,
                          populationNames = NULL,
                          overwrite = FALSE)


  # Simulations ------------------------------------------------------
  # set up Scenarios

  # initialize  all scenarios previously defined in scenario.xlsx
  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = NULL,
                            doCheckScenarioNameValidity = TRUE)

  # calculate PK Parameter

  # run initialized scenarios
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)


  pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                              scenarioList = scenarioList)


  # Create Output Plots -----------------------------------------------------
  # (see vignette OSPSuite_ReportingFramework.Rmd  section  Plot Functionality)
  # figures and captions are filed in <rootdirectory>\Outputs\ReportingFramework\TimeProfiles

  ## Case 1 different Populations --------
  ### Box whisker  --------
  addDefaultConfigForPKBoxwhsikerPlots(projectConfiguration = projectConfiguration,
                                                   pkParameterDT = pkParameterDT,
                                                   sheetName = "PKParameter_Boxplot",
                                                   overwrite = TRUE)
  mockManualEditings.PlotBoxwhsiker1(projectConfiguration)

  runPlot(
    nameOfplotFunction = "plotPKBoxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot1",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  ### Forest  --------
  addDefaultConfigForPKForestPlots(projectConfiguration = projectConfiguration,
                                   pkParameterDT = pkParameterDT,
                                   sheetName = "PKParameter_ForestAbs1",
                                   overwrite = TRUE)

  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbs1",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbsCI1",
    inputs = list(
      pkParameterDT = pkParameterDT,
      nObservationDefault = 16,
      relWidth = c(3,1)
    )
  )

  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatio1",
    inputs = list(
      pkParameterDT = pkParameterDT,
      aggregationFlag = "Percentiles" # "ArithmeticStdDev" # "GeometricStdDev",
    )
  )

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatioCI1",
    inputs = list(
      pkParameterDT = pkParameterDT,
      nObservationDefault = 16
    )
  )



  ## Case 2 same Populations --------
  ### Box whisker same Populations --------
  addDefaultConfigForPKBoxwhsikerPlots(projectConfiguration = projectConfiguration,
                                       pkParameterDT = pkParameterDT,
                                       sheetName = "PKParameter_Boxplot2",
                                       overwrite = TRUE)
  mockManualEditings.PlotBoxwhsiker2(projectConfiguration)


  runPlot(
    nameOfplotFunction = "plotPKBoxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot2",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )

  ## Forest same Populations --------
  addDefaultConfigForPKForestPlots(projectConfiguration = projectConfiguration,
                                       pkParameterDT = pkParameterDT,
                                       sheetName = "PKParameter_ForestAbs2",
                                       overwrite = TRUE)
  mockManualEditings.PlotForest2(projectConfiguration)

  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbs2",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )


  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteValues",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestAbsCI2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      nObservationDefault = 16
    )
  )


  runPlot(
    nameOfplotFunction = "plotPKForestAggregatedRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatio2",
    inputs = list(
      pkParameterDT = pkParameterDT
    )
  )


  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatioCI2",
    inputs = list(
      pkParameterDT = pkParameterDT,
      coefficientOfVariation = 0.3,
      nObservationDefault = 16,
      pkParameterObserved = dataObservedPK
    )
  )

# test same population whit algorithm for different -----------
  pkParameterDTTest <- copy(pkParameterDT)
  pkParameterDTTest[grep('_po$',pkParameterDT2$scenarioName),populationId := paste0(populationId,'_po')]

  runPlot(
    nameOfplotFunction = "plotPKBoxwhisker",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_Boxplot2",
    rmdName = "PKParameter_Boxplot2_Test",
    inputs = list(
      pkParameterDT = pkParameterDTTest
    )
  )

  runPlot(
    nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteRatios",
    projectConfiguration = projectConfiguration,
    configTableSheet = "PKParameter_ForestRatioCI2",
    rmdName = "PKParameter_ForestRatioCI2_Test",
    inputs = list(
      pkParameterDT = pkParameterDTTest,
      nObservationDefault = 16,
      pkParameterObserved = dataObservedPK
    )
  )



  source(system.file(
    "extdata", "example_2", "compareCsvAlgorithmTest.R",
    package = "ospsuite.reportingframework",
    mustWork = TRUE
  ))

  runPlot(
    nameOfplotFunction = "plotMethodComparison",
    projectConfiguration = projectConfiguration,
    rmdName = 'MethodTest',
    configTableSheet = NULL,
    inputs = list(
    )
  )


  # Create Report document --------------------------------------------------
  mergeRmds(projectConfiguration = projectConfiguration,
            newName = "appendix",
            title = "Appendix",
            sourceRmds = c("PKParameter_Boxplot1",
                           "PKParameter_ForestAbs1",
                           "PKParameter_ForestAbsCI1",
                           "PKParameter_ForestRatio1",
                           "PKParameter_ForestRatioCI1",
                           "PKParameter_Boxplot2",
                           "PKParameter_ForestAbs2",
                           "PKParameter_ForestAbsCI2",
                           "PKParameter_ForestRatio2",
                           "PKParameter_ForestRatioCI2"
                           )
  )

  renderWord(fileName = file.path(projectConfiguration$outputFolder,"appendix.Rmd"))


#})

# finalize workflow---------------------
addMessageToLog("finalize workflow")

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()

