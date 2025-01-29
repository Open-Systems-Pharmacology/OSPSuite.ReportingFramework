# Purpose: Test case and base for Tutorial
#
source(system.file(
  "extdata", "example_1", "mockManualEditings.R",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))

# Initialization  ----------------------------------------------------------

# set graphic all defaults
# (see vignette vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')

# Set this to TRUE if you want to execute the workflow as a final valid run.
# It then won't set watermarks to figures and does not skip failing plot generations
# (see vignette OSPSuite_ReportingFramework)
setWorkflowOptions(isValidRun = FALSE)
options(OSPSuite.RF.skipFailingPlots = FALSE)

# Setup project structure -------------------------------------------------
# creates project directory (see vignette https://esqlabs.github.io/esqlabsR/articles/esqlabsR-project-structure.html)
# and help initProject
# if you go with default structure this workflow file should be saved in Scripts/ReportingFramework,
initProject()

# available are two model files  for an IV and a po administration  "iv 1 mg (5 min).pkml" "po 3 mg (solution).pkml"
# and a data file 'observedData_drugX.csv' for a cross over study- This contains plasma concentration and fraction excreted to Urine for 7 individuals
# for both applications.
# All files are filed in the inst/extdata file of the package

# copy files needed for tutorials to the correct folders

file.copy(from = system.file(
  "extdata","example_1","iv 1 mg (5 min).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"iv 1 mg (5 min).pkml"),overwrite = TRUE)

file.copy(from = system.file(
  "extdata","example_1","po 3 mg (solution).pkml",
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Models',"po 3 mg (solution).pkml"),overwrite = TRUE)

dir.create(file.path('..','..','Data'))
file.copy(from = system.file(
  "extdata","example_1",'observedData_drugX.csv',
  package = "ospsuite.reportingframework",
  mustWork = TRUE),
  to = file.path('..','..','Data','observedData_drugX.csv'),overwrite = TRUE)


# get paths of all relevant project files
projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

# start log Catch loop which catches all errors, warnins and messages in a logfile
# (see vignette OSPSuite_ReportingFramework)
logCatch({

  # initialize log file
  initLogfunction(projectConfiguration = projectConfiguration)

  for (tutorialstep in seq(1,5)){

    message(tutorialstep)

    if (tutorialstep == 1){
      # Read observedData -------------------------------------------------------
      # (see vignette('Data_import_by_dictionary'))
      mockManualEditings.DataDictionary(projectConfiguration)
    }
    # read observed data
    dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)

    if (tutorialstep >= 3){
      # add aggregated  groups of data
      dataObserved <- rbind(dataObserved,
                            aggregatedObservedDataGroups(dataObserved = dataObserved,
                                                         groups = c('IV','PO')),
                            fill = TRUE
      )
    }
    if (tutorialstep == 3){
      # for the manually added groups the configuration sheet in Plot.xlsx has to be updated
      updateDataGroupId(projectConfiguration, dataObserved)
    }

    # export populations ------------------------------------------------------
    # (see vignette Simulation_setup)
    if (tutorialstep == 2){

      mockManualEditings.Population(projectConfiguration,dataObserved,tutorialstep = tutorialstep)


      # exports all populations defined population.xlsx sheet "IndividualPopulation"
      # model file is used for unit conversion,
      # as typical population parameter are not dependend an application both model
      # files of the project (iv and po) can be used
      exportVirtualTwinPopulations(
        projectConfiguration = projectConfiguration,
        populationNames = NULL,
        modelFile = "po 3 mg (solution).pkml",
        overwrite = FALSE
      )

      # generated output is file here: rootdirectory\Models\Populations\virtualtwin_population.csv
    }

    if (tutorialstep == 3){

      mockManualEditings.Population(projectConfiguration,dataObserved,tutorialstep = tutorialstep)

      # exports all populations defined in population.xlsx sheet "Demographics"
      exportRandomPopulations(projectConfiguration = projectConfiguration,
                              populationNames = NULL,
                              overwrite = FALSE)
    }


    # Simulations ------------------------------------------------------
    # set up Scenarios
    mockManualEditings.Scenario(projectConfiguration,dataObserved,tutorialstep)


    if (tutorialstep == 1){
      # initialize  all scenarios previously defined in scenario.xlsx
      scenarioList <-
        createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                scenarioNames = NULL,
                                doCheckScenarioNameValidity = TRUE)
    }
    if (tutorialstep == 2){
      # initialize newly added scenarios
      scenarioList <-
        createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                scenarioNames = c('virtual_twin_population_iv','virtual_twin_population_po'),
                                doCheckScenarioNameValidity = TRUE)
    }
    if (tutorialstep == 3){
      # initialize newly added scenarios
      scenarioList <-
        createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                scenarioNames = c('random_population_iv','random_population_po'),
                                doCheckScenarioNameValidity = TRUE)
    }
    if (tutorialstep == 4){
      # initialize newly added scenarios
      scenarioList <-
        createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                scenarioNames = c('reference_population_iv','reference_population_po'),
                                doCheckScenarioNameValidity = TRUE)
    }

    if (tutorialstep <= 4)
      # run initialized scenarios
      scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                          scenarioList = scenarioList,
                          simulationRunOptions = SimulationRunOptions$new(
                            numberOfCores = NULL,
                            checkForNegativeValues = NULL,
                            showProgress = TRUE
                          ))


    # outputs filed in <rootdirectory>Outputs\ReportingFramework\SimulationResults

    # Create Output Plots -----------------------------------------------------
    # (see vignette OSPSuite_ReportingFramework.Rmd  section  Plot Functionality)
    if (tutorialstep <= 3)
      mockManualEditings.DataGroups(projectConfiguration = projectConfiguration,dataObserved = dataObserved,tutorialstep)

    # Timeprofile Plots
    # see vignette TimeProfilePlots

    #prepare configtable adds missing scenarios tp plot configuration with default settings
    if (tutorialstep <= 4)
      addDefaultConfigForTimeProfilePlots(
      projectConfiguration = projectConfiguration,
      sheetName = 'TimeProfiles',
      overwrite = FALSE
    )
      mockManualEditings.TimePlot(projectConfiguration = projectConfiguration,dataObserved = dataObserved,tutorialstep)

  }


  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration,
                            scenarioNames = NULL,
                            doCheckScenarioNameValidity = TRUE)
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(showProgress = TRUE),
                                         withResimulation = FALSE)

  mockManualEditings.DataGroups(projectConfiguration = projectConfiguration,dataObserved = dataObserved,tutorialstep)

  runPlot(
    functionKey = "plotTimeProfiles",
    projectConfiguration = projectConfiguration,
    configTableSheet = "TimeProfiles",
    inputs = list(
      dataObserved = dataObserved,
      scenarioResults = scenarioResults
    )
  )

  # figures and captions are filed in <rootdirectory>\Outputs\ReportingFramework\TimeProfiles


  # Create Report document --------------------------------------------------
  mergeRmds(projectConfiguration = projectConfiguration,
            newName = "appendix",
            title = "Appendix",
            sourceRmds = c("TimeProfiles")
  )

  renderWord(fileName = file.path(projectConfiguration$outputFolder,"appendix.Rmd"))


})

# finalize workflow---------------------
addMessageToLog("finalize workflow")

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()

