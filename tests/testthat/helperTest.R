#' Build Test Project Directory
#'
#' This function sets up a test project directory for the `ospsuite.reportingframework` package.
#' It initializes the project structure, configures scenarios, and handles necessary file
#' synchronization for testing and development purposes. It can be used in both development
#' mode and testing environments.
#'
#' @param rootDirectory A character string specifying the root directory for the test project.
#'                     If NULL (default), a temporary directory will be created.
#' @param verbose A logical value indicating whether to print messages about the process.
#'                Defaults to FALSE.
#' @param writeTestData A logical value indicating whether newly created testdata should be filed to the inst directory of the package.
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @details
#' The function performs the following steps:
#' 1. Initializes the project structure.
#' 2. Cleans up existing data and sets up mock manual edits on project configuration.
#' 3. Synchronizes directories for models and populations.
#' 4. Exports defined populations and runs simulations for scenarios.
#' 5. In development mode, it synchronizes simulation results and PK analysis results back
#'    to the development directory.
buildTestData <- function(rootDirectory = NULL,
                          writeTestData = FALSE) {
  # Initialize class to build test project
  pBuilder <- TestProjectBuilder$new()

  # get projectConfiguration
  projectConfiguration <- pBuilder$iniTestProject(rootDirectory = rootDirectory)

  # Clear existing data from relevant Excel sheets
  pBuilder$mockManualEditingsCleanup(projectConfiguration)

  # Define virtual populations within biometric ranges
  randomPops <- data.table(
    populationName = c("adults", "toddler", "children", "school-children", "adolescents"),
    species = "Human",
    population = "European_ICRP_2002",
    numberOfIndividuals = 100,
    proportionOfFemales = 50,
    ageMin = c(20, 0.5, 2, 6, 12),
    ageMax = c(40, 2, 6, 12, 18),
    weightUnit = "kg",
    heightUnit = "cm",
    bMIUnit = "kg/m\u00B2",
    protein = "CYP3A4,UGT1A4",
    ontogeny = "CYP3A4,UGT1A4"
  )
  pBuilder$mockManualEditingsPopulation(projectConfiguration,
                                        randomPops = randomPops
  )

  # Set up scenarios based on the defined populations, add PK Parameter and output paths
  pBuilder$mockManualEditingsScenario(projectConfiguration,
                                      dtTestScenarios = getTestScenarios(projectConfiguration),
                                      pKParameter = "PK_Plasma, PK_Fraction"
  )

  # Add PK parameters to the project configuration
  pBuilder$mockManualEditingsPKParameter(projectConfiguration)

  pBuilder$setupRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = unique(randomPops$populationName),
    writeTestData = writeTestData
  )

  # Initialize all scenarios previously defined in scenario.xlsx
  scenarioList <- createScenarios.wrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = NULL
  )

  # Run scenarios and calculate PK
  scenarioResults <- pBuilder$setupSimulations(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    writeTestData = writeTestData
  )

  # generate Random data
  setupRandomDataForTest(
    pBuilder = pBuilder,
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    scenarioResults = scenarioResults
  )

  # generate virtual twin populations
  scenarioListInd <-
    setupVirtualTwinScenariosForTest(
      pBuilder = pBuilder,
      projectConfiguration = projectConfiguration
    )

  # Run scenarios and calculate PK
  scenarioResultsInd <- pBuilder$setupSimulations(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioListInd,
    writeTestData = writeTestData
  )

  # Setup sensitivity
  projectConfiguration <- pBuilder$setUpSensitivity(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioListInd,
    scenarioName = "i123413_iv",
    parameterList = list(
      "DrugX Lipophilicity" = "DrugX Lipophilicity",
      "DrugX Fraction unbound" = "DrugX Fraction unbound",
      "CYP3A4 Ontogeny factor" = "CYP3A4 Ontogeny factor"
    ),
    sensitivitySheet = "smallSelection",
    writeTestData = writeTestData
  )

  return(invisible(list(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    scenarioResults = scenarioResults,
    scenarioListInd = scenarioListInd,
    scenarioResultsInd = scenarioResultsInd
  )))
}

expectDoppelgangerLoop <- function(plotList){

  for (pName in names(plotList)) {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }
}

## auxiliaries --------
#' Setup Random Data for Test
#'
#' This function adds random pharmacokinetic (PK and time profile) data to the test project,
#' including shifting values for selected individuals.
#'
#' @param pBuilder An TestProjectBuilder object, which provides function to build the test project
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @param scenarioList A list of scenarios initialized for the project.
#' @param scenarioResults A list containing results from running the scenarios.
#'
#' @return Invisible NULL.
#' @keywords internal
setupRandomDataForTest <- function(pBuilder, projectConfiguration, scenarioList, scenarioResults) {
  pkParameterDT <- loadPKParameter(
    projectConfiguration = projectConfiguration,
    scenarioListOrResult = scenarioList
  )

  set.seed(123) # Set seed for reproducibility
  ids <- sample(unique(pkParameterDT$individualId), 6, replace = FALSE)

  pBuilder$addRandomTPData(
    projectConfiguration = projectConfiguration,
    scenarioResults = scenarioResults[c("adults_iv", "adults_po")],
    ids = ids
  )

  pBuilder$addRandomPKData(
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT,
    ids = ids
  )

  return(invisible())
}
#' Get Test Scenarios
#'
#' This function generates test scenarios based on the provided project configuration.
#' It reads population data from a specified Excel file and constructs a data table
#' with scenario names, population IDs, model files, and output path IDs.
#'
#' @param projectConfiguration A list containing the configuration for the project,
#'                             including the path to the populations file.
#'
#' @return A data.table containing the scenarios
getTestScenarios <- function(projectConfiguration) {
  # initialize variable to avoid messages
  scenario_name <- NULL #nolint
  longName <- shortName <- outputPathsIds <- NULL

  instDirectory <- system.file(
    package = "ospsuite.reportingframework",
    "extdata",
    mustWork = TRUE
  )

  modelFiles <- list.files(file.path(instDirectory, "Models"))
  names(modelFiles) <- substr(modelFiles, 1, 2)

  dtPop <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")

  dtScenario <- rbind(
    data.table(
      scenario_name = paste0(gsub("-", "_", dtPop$populationName), "_iv"),
      populationId = dtPop$populationName,
      readPopulationFromCSV = 1,
      modelFile = modelFiles["iv"]
    ),
    data.table(
      scenario_name = paste0(gsub("-", "_", dtPop$populationName), "_po"),
      populationId = dtPop$populationName,
      readPopulationFromCSV = 1,
      modelFile = modelFiles["po"]
    ),
    fill = TRUE
  )

  dtScenario[, outputPathsIds := "Plasma, CYP3A4total, CYP3A4Liver"]
  dtScenario[, shortName := gsub("_", " ", gsub("_iv", "", gsub("_po", "", scenario_name)))]
  dtScenario[, longName := gsub("_", " ", gsub("_iv", "", gsub("_po", "", scenario_name)))]

  return(dtScenario)
}


#' Setup Virtual Twin Scenarios for Test
#'
#' This function sets up virtual twin scenarios for the test project based on the defined populations
#' and model files.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#'
#' @return A list of scenario names initialized for the virtual twin scenarios.
#' @keywords internal
setupVirtualTwinScenariosForTest <- function(pBuilder, projectConfiguration) {
  # initialize variable to avoid messages
  modelFile <- readPopulationFromCSV <- NULL
  scenario_name <- NULL #nolint

  invisible(readObservedDataByDictionary(projectConfiguration))

  dtVirtualTwin <- pBuilder$mockManualEditingsIndividuals(projectConfiguration)

  individualIds <- dtVirtualTwin$individualId

  # add a scenarios
  dtTestScenarios <- rbind(
    data.table(
      scenario_name = "p_1234_adults_iv",
      populationId = "1234_adults",
      readPopulationFromCSV = 1,
      individualId = ""
    ),
    data.table(
      scenario_name = "p_1234_adults_po",
      populationId = "1234_adults",
      readPopulationFromCSV = 1,
      individualId = ""
    ),
    data.table(
      scenario_name = paste(tolower(individualIds), "iv", sep = "_"),
      populationId = "",
      readPopulationFromCSV = 0,
      individualId = individualIds
    )
  )
  dtTestScenarios[readPopulationFromCSV == 1, `:=`(
    shortName = "study 1234",
    longName = "study 1234 iv application"
  )]
  dtTestScenarios[readPopulationFromCSV == 0, `:=`(
    shortName = paste("individual", gsub("i1234", "", gsub("_iv", "", scenario_name))),
    longName = paste("individual", gsub("i1234", "", gsub("_iv", "", scenario_name)), "iv application")
  )]

  modelFiles <- list.files(projectConfiguration$modelFolder, pattern = ".pkml")
  dtTestScenarios[grep("_iv$", scenario_name), modelFile := grep("^iv", modelFiles, value = TRUE)]
  dtTestScenarios[grep("_po$", scenario_name), modelFile := grep("^po", modelFiles, value = TRUE)]

  pBuilder$mockManualEditingsScenario(
    projectConfiguration = projectConfiguration,
    dtTestScenarios = dtTestScenarios
  )

  exportVirtualTwinPopulations(
    projectConfiguration = projectConfiguration,
    modelFile = list.files(projectConfiguration$modelFolder, pattern = ".pkml")[1],
    overwrite = TRUE
  )


  scenarioListInd <- suppressWarnings(createScenarios.wrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = dtTestScenarios$scenario_name
  ))

  return(scenarioListInd)
}
#' Mock Manual Editings for PK Forest Test Plots
#'
#' This function modifies the PKParameter_ForestTest sheet in the provided Excel workbook
#' according to specific rules for pediatric and crossover data.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @return Returns invisible NULL.
mockManualEditingsPlotPkForestTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  plotName <- scenario <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  dt <- xlsxReadData(wb = wb, sheetName = "PKParameter_ForestTest")

  dt <- dt[!c(grep("_iv$", scenario)), ]
  dt <- dt[!c(grep("^p_", scenario)), ]
  dt[plotName == "PKForest", `:=`(
    plotName = "pediatric",
    scenarioGroup = "Pediatric",
    referenceScenario = "adults_iv",
    plotCaptionAddon = "Pediatric study."
  )]
  dt[plotName == "pediatric" & scenario == "adults_po", `:=`(
    plotName = "pediatric",
    scenarioGroup = "Adult",
    referenceScenario = ""
  )]

  dtnew <- dt[plotName == "pediatric"]
  dtnew[, `:=`(
    plotName = "pediatric_data",
    dataGroupId = paste0("1234_", scenario),
    facetScale = "free"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "pediatric"]
  dtnew[, `:=`(
    plotName = "crossover",
    referenceScenario = gsub("_po", "_iv", scenario),
    facetScale = "free"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "crossover"]
  dtnew[, `:=`(
    plotName = "crossover_data",
    dataGroupId = paste0("1234_", gsub("_po", "", scenario), "_ratio"),
    facetScale = "free"
  )]

  dt <- rbind(dt, dtnew)


  xlsxWriteData(wb = wb, sheetName = "PKParameter_ForestTest", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}
#' Mock Manual Editings for Boxplot Test
#'
#' This function modifies the PKParameter_BoxplotTest sheet in the provided Excel workbook
#' according to specific rules for crossover and pediatric data.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @return Returns invisible NULL.
mockManualEditingsPlotBoxwhsikerTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  plotName <- referenceScenario <- scenario <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb, sheetName = "PKParameter_BoxplotTest")

  dt <- dt[grep("C_max$", plotName), plotName := "crossover"]
  dt <- dt[!intersect(grep("_iv$", scenario), grep("crossover", plotName))]
  dt <- dt[!c(grep("^p_", scenario)), ]
  dt[plotName == "crossover", `:=`(
    referenceScenario = gsub("_po", "_iv", scenario),
    colorLegend = "PO Administration| IV Administration",
    plotCaptionAddon = "IV Application in comparison to PO administration",
    yScale = "log",
    ylimit_log = "c(0.08,NA)",
    plot_Absolute = 0,
    plot_Ratio = 1
  )]

  dt <- dt[grep("F_tEnd$", plotName), plotName := "pediatric"]
  dt <- dt[!intersect(grep("_iv$", scenario), grep("pediatric", plotName))]
  dt[plotName == "pediatric", `:=`(
    referenceScenario = "adults_po",
    colorLegend = "pediatric| adult",
    plotCaptionAddon = "Pediatric IV Application in comparison to adults",
    facetScale = "free_y",
    plot_Ratio = 0
  )]
  dt[scenario == "adults_po" & plotName == "pediatric", referenceScenario := ""]


  xlsxCloneAndSet(wb = wb, clonedSheet = "PKParameter_Boxplot", sheetName = "PKParameter_BoxplotTest", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
  return(invisible())
}
#' Mock Manual Editings for Demographics Test
#'
#' This function modifies the HistogramTest and RangePlotTest sheets in the provided Excel workbook
#' according to specific rules for demographics and PK parameters.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @return Returns invisible NULL.
mockManualEditingsPlotDemographicsTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  colorLegend <- referenceScenario <- scenarios <- scenario <- plotName <- NULL

  # prepare configtable
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb, sheetName = "HistogramTest")

  # demographic histogram
  dt <- dt[!(intersect(
    grep("demographics", plotName),
    c(
      grep("adults", scenario),
      grep("i1234", scenario)
    )
  ))]
  dt <- dt[!c(grep("^p_", scenario)), ]
  dt[plotName == "demographics", `:=`(
    parameterIds = "weight,gender",
    referenceScenario = "adults_iv",
    colorLegend = "pediatrics|adults",
    xlimit_linear = "c(0,NA)",
    plotCaptionAddon = "Pediatric virtual populations in comparison to an adult virtual population"
  )]


  # Add Test for histogram onPKParameter
  dt <- dt[!grep("pkparameter1", plotName)]
  dt <- dt[!(intersect(
    grep("pkparameter2", plotName),
    grep("iv", scenario)
  ))]
  dt[plotName == "pkparameter2", `:=`(
    referenceScenario = gsub("po", "iv", scenario),
    colorLegend = "IV application | PO application",
    plotCaptionAddon = "Comparison of IV and PO application"
  )]

  xlsxWriteData(wb = wb, sheetName = "HistogramTest", dt = dt)

  dt <- xlsxReadData(wb = wb, sheetName = "RangePlotTest")

  # Update relevant fields for 'demographics' range plot test
  dt[plotName == "demographics", `:=`(
    scenarios = "toddler_iv, children_iv, school_children_iv, adolescents_iv",
    referenceScenario = "adults_iv",
    colorLegend = "pediatrics|adults",
    ylimit_linear = "c(0,NA)",
    modeOfBinning = BINNINGMODE$breaks,
    numberOfBins = "seq(0,18)",
    plotCaptionAddon = "Pediatric virtual populations in comparison to an adult virtual population"
  )]

  # Update relevant fields for 'pk' range plot test
  dt <- dt[!grepl("pkparameter1", plotName)]

  dt[plotName == "pkparameter2", `:=`(
    scenarios = dt[plotName == "demographics", scenarios],
    referenceScenario = dt[plotName == "demographics", referenceScenario],
    colorLegend = dt[plotName == "demographics", colorLegend],
    facetScale = "free_y",
    plotCaptionAddon = "IV Application for pediatric virtual simulations in comparison to an adult virtual simulation"
  )]


  xlsxWriteData(wb = wb, sheetName = "RangePlotTest", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}
#' Mock Manual Editings for Sensitivity Test
#'
#' This function modifies the SensitivityPlots sheet in the provided Excel workbook
#' according to specific rules for sensitivity analysis.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @param sensitivityScenario A string representing the sensitivity scenario.
#' @param sensitivitySheet A string representing the sensitivity sheet.
#' @return Returns invisible NULL.
mockManualEditingsPlotSensitivityTest <- function(projectConfiguration,
                                                  sensitivityScenario,
                                                  sensitivitySheet) {
  # initialize variable to avoid messages
  plotName <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  sheetName <- "SensitivityPlots"
  dt <- xlsxReadData(wb = wb, sheetName = sheetName)
  dt <- dt[1]

  dt <- rbind(dt,
    data.table(level = 1, header = "Sensitivity Analysis"),
    data.table(
      plotName = "sensitivity_all",
      scenario = sensitivityScenario,
      outputPathIds = "Plasma",
      sensitivityParameterSheet = sensitivitySheet,
      pKParameters = "C_max, AUC_inf",
      threshold = 1,
      plotCaptionAddon = "Displayed are all model parameters."
    ),
    fill = TRUE
  )
  dt <- rbind(
    dt,
    dt[plotName == "sensitivity_all"] %>%
      dplyr::mutate(
        plotName = "sensitivity_90",
        threshold = 0.9,
        plotCaptionAddon = "Displayed are  model parameters participating to a total of `90` percent of the sensitivity ."
      )
  )

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
#' Mock Manual Editings for Time Profile Test
#'
#' This function modifies the TimeProfileTest sheet in the provided Excel workbook
#' and adds new time range tags.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @return Returns invisible NULL.
mockManualEditingsPlotTimeProfileTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  plotName <- scenario <- individualIds <- NULL
  timeRange_h0_6 <- timeRange_h6_24 <- NULL # nolint used with tags

  sheetName <- "TimeProfileTest"
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # create new timerange tags
  dtTimeRange <- xlsxReadData(wb, sheetName = "TimeRange")
  dtTimeRange
  dtTimeRange <- rbind(
    dtTimeRange,
    data.table(
      tag = "h0_6",
      captionText = "Zoom on first 6 hours",
      timeLabel = "Time",
      timeShift = 0
    ),
    data.table(
      tag = "h6_24",
      captionText = "Zoom on time range 6 to 24 hours",
      timeLabel = "Time after dose",
      timeShift = 6
    )
  ) %>%
    unique()

  xlsxWriteData(wb = wb, sheetName = "TimeRange", dt = dtTimeRange)


  dt <- xlsxReadData(wb = wb, sheetName = sheetName)

  # add custom time ranges
  dt[, timeRange_h0_6 := as.character(timeRange_h0_6)]
  dt[, timeRange_h0_6 := NA_character_]
  dt[, timeRange_h6_24 := as.character(timeRange_h6_24)]
  dt[, timeRange_h6_24 := NA_character_]


  # individual data
  dt[grep("i1234", scenario), `:=`(
    plotCaptionAddon = "IV application",
    timeRange_firstApplication = NA,
    timeRange_lastApplication = NA
  )]
  dt[grep("i1234", scenario)[c(1, 2, 3)], `:=`(
    plotName = "Individuals_withData",
    outputPathIds = "Plasma, (CYP3A4total, CYP3A4Liver)",
    yScale = "linear",
    dataGroupIds = "1234_adults_iv",
    individualIds = toupper(gsub("_iv", "", scenario)),
    nFacetColumns = 2
  )]
  dt[grep("i1234", scenario)[c(4, 5, 6)], `:=`(
    plotName = "Individuals_withoutData",
    nFacetColumns = 3,
    yScale = "log"
  )]

  dtnew <- dt[plotName == "Individuals_withData"]
  dtnew[, `:=`(
    plotName = "Individuals_withData_pvo",
    outputPathIds = "(CYP3A4total, CYP3A4Liver)",
    plot_TimeProfiles = 0,
    foldDistance_PvO = 1.5,
    plot_PredictedVsObserved = 1,
    plot_ResidualsAsHistogram = 1,
    plot_ResidualsVsObserved = 1,
    plot_ResidualsVsTime = 1,
    plot_QQ = 1
  )]

  dt <- rbind(dt, dtnew)

  # virtualpop
  dt[scenario == "p_1234_adults_iv", `:=`(
    plotCaptionAddon = "IV application",
    timeRange_firstApplication = NA,
    timeRange_lastApplication = NA,
    individualIds = "*",
    plotName = "VirtualTwin",
    outputPathIds = "Plasma",
    yScale = "linear",
    nFacetColumns = 3
  )]

  dtnew <- dt[plotName == "VirtualTwin"]
  dtnew[, `:=`(
    plotName = "VirtualTwin_withData_all",
    outputPathIds = "Plasma",
    yScale = "log",
    dataGroupIds = "1234_adults_iv",
    facetScale = "free",
    timeUnit = "min"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "VirtualTwin"]
  dtnew[, `:=`(
    plotName = "VirtualTwin_withData_selected",
    outputPathIds = "Plasma",
    yScale = "log",
    ylimit_log = "c(0.01,100)",
    dataGroupIds = "1234_adults_iv",
    individualIds = paste(dt[plotName == "Individuals_withData"]$individualIds, collapse = ", ")
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "VirtualTwin_withData_selected"]
  dtnew[, `:=`(
    plotName = "VirtualTwin_withReferenceInd",
    referenceScenario = dt[grep("i1234", scenario)[4]]$scenario,
    colorLegend = "test individual | reference individual"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "VirtualTwin_withData_selected"]
  dtnew[, `:=`(
    plotName = "VirtualTwin_withReferencePop",
    referenceScenario = "adults_iv",
    colorLegend = "test individual | reference population"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "VirtualTwin_withData_selected"]
  dtnew[, `:=`(
    plotName = "VirtualTwin_withReferenceTwinPop",
    referenceScenario = "p_1234_adults_po",
    colorLegend = "iv | po"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "VirtualTwin_withData_selected"]
  dtnew[, `:=`(
    plotName = "VirtualTwin_withData_selected_pvo",
    individualIds = paste0("(", individualIds, ")"),
    outputPathIds = "Plasma",
    plot_TimeProfiles = 0,
    foldDistance_PvO = 1.5,
    plot_PredictedVsObserved = 1
  )]

  dt <- rbind(dt, dtnew)


  dt[scenario == "toddler_iv", `:=`(
    plotName = "Pop_withoutData",
    plotCaptionAddon = "IV application",
    timeRange_firstApplication = NA,
    timeRange_lastApplication = NA,
    outputPathIds = "Plasma, CYP3A4total, CYP3A4Liver",
    yScale = "linear",
    nFacetColumns = 3
  )]

  dt[scenario == "adults_iv", `:=`(
    plotName = "Pop_withIndividualData",
    plotCaptionAddon = "IV application",
    timeRange_firstApplication = NA,
    timeRange_lastApplication = NA,
    outputPathIds = "Plasma, (CYP3A4total, CYP3A4Liver)",
    yScale = "log",
    dataGroupIds = "1234_adults_iv",
    nFacetColumns = 2
  )]

  dtnew <- dt[plotName == "Pop_withIndividualData"]
  dtnew[, `:=`(
    plotName = "Pop_withAggregatedData",
    dataGroupIds = "1234_adults_iv_aggregated"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "Pop_withIndividualData"]
  dtnew[, `:=`(
    plotName = "Pop_withAggregatedData_Percentiles",
    dataGroupIds = "1234_adults_po_aggregated"
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "Pop_withAggregatedData"]
  dtnew[, `:=`(
    plotName = "Pop_withAggregatedData_pvo",
    outputPathIds = "Plasma",
    plot_TimeProfiles = 0,
    plot_PredictedVsObserved = 1
  )]

  dt <- rbind(dt, dtnew)


  dtnew <- dt[plotName == "Pop_withoutData"]
  dtnew[, `:=`(
    plotName = "Pop_withReference",
    outputPathIds = "Plasma, CYP3A4total",
    referenceScenario = "adults_iv",
    colorLegend = "toddler | adults",
    yScale = "log",
    timeOffset = -4,
    timeOffset_Reference = -6
  )]

  dt <- rbind(dt, dtnew)

  dtnew <- dt[plotName == "Pop_withReference"]
  dtnew[, `:=`(
    plotName = "Pop_withReference_2",
    outputPathIds = "Plasma, (CYP3A4total, CYP3A4Liver)",
    referenceScenario = "adults_iv",
    colorLegend = "toddler | adults"
  )]
  dt <- rbind(dt, dtnew)


  dtnew <- dt[plotName == "Pop_withoutData"]
  dtnew[, `:=`(
    plotName = "Pop_withTimeRanges",
    timeRange_h0_6 = "c(0,6)",
    timeRange_h6_24 = "c(6,24)",
    splitPlotsPerTimeRange = 0
  )]
  dt <- rbind(dt, dtnew)


  dtnew <- dt[plotName == "Pop_withIndividualData"]
  dtnew[, `:=`(
    plotName = "Pop_withTimeRanges_pvo",
    timeRange_h0_6 = "c(0,6)",
    timeRange_h6_24 = "c(6,24)",
    splitPlotsPerTimeRange = 0,
    outputPathIds = "Plasma",
    plot_TimeProfiles = 0,
    foldDistance_PvO = 1.5,
    plot_PredictedVsObserved = 1
  )]
  dt <- rbind(dt, dtnew)

  #keep only adjusted entries
  dt[is.na(timeRange_firstApplication) | scenario != plotName]

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)


  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
#' Cleanup Electronic Package Folder
#'
#' This function removes the electronic package folder specified in the project configuration.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @return Returns invisible NULL.
cleanupElectronicPackage <- function(projectConfiguration) {
  packageFolder <- suppressWarnings(projectConfiguration$addOns$electronicPackageFolder)
  if (dir.exists(packageFolder)) {
    unlink(packageFolder, recursive = TRUE) # Remove the folder and its contents
  }
}
#' Generate Mock R Markdown File
#'
#' This function creates an R Markdown file for testing the TLF workflow export.
#'
#' @param projectConfiguration A list containing configuration settings for the project.
#' @param codeChunkList A list of code chunks to include in the R Markdown file.
#' @param filename A string representing the name of the output R Markdown file.
#' @return Returns invisible NULL.
generateMockRmd <- function(projectConfiguration, codeChunkList, filename) {
  codeChunkList <- utils::modifyList(
    list(
      "dataObserved-eval" = "dataObserved <- NULL",
      "dataObservedPK-eval" = "dataObservedPK <- NULL"
    ),
    codeChunkList
  )


  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 99,
    workflowRmd = system.file("templates", "template_ePackageWorkflow.Rmd",
      package = "ospsuite.reportingframework"
    )
  )

  # Step 2: Call the extractCodeChunks method
  exporter$extractCodeChunks()

  codeChunkList <- utils::modifyList(
    exporter$codeChunks,
    codeChunkList
  )

  contentLines <- c(
    "---",
    'title: "Workflow for Report generation"',
    "output: html_document",
    "---",
    "  ",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE)",
    "knitr::opts_chunk$set(cache = TRUE)",
    "```"
  )

  for (codeChunk in names(codeChunkList)) {
    contentLines <- c(
      contentLines,
      "",
      paste0("```{r ", codeChunk, "}"),
      codeChunkList[[codeChunk]],
      "```"
    )
  }

  writeLines(contentLines, filename)

  return(invisible())
}
