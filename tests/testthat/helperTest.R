#' Setup Test Directory for Testing
#'
#' This function sets up a test directory specifically for use in unit tests with
#' the `testthat` framework.
#'
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @export
setupTestDirectoryForTests <- function() {
  message("load test project")
  buildTestData(rootDirectory = NULL, verbose = FALSE, writeTestData = FALSE)
}

mockManualEditingsPlotPkForestTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  plotName <- scenario <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  dt <- xlsxReadData(wb = wb, sheetName = "PKParameter_ForestTest")

  dt <- dt[!c(grep("_iv$", scenario)), ]
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

# adjust configtables in project directory
mockManualEditingsPlotBoxwhsikerTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  plotName <- referenceScenario <- scenario <- NULL

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb, sheetName = "PKParameter_BoxplotTest")

  dt <- dt[grep("C_max$", plotName), plotName := "crossover"]
  dt <- dt[!intersect(grep("_iv$", scenario), grep("crossover", plotName))]
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
mockManualEditingsPlotTimeProfileTest <- function(projectConfiguration) {
  # initialize variable to avoid messages
  plotName <- scenario <- individualIds <- NULL
  timeRange_h0_6 <- timeRange_h6_24 <- NULL # nolint used with tags

  sheetName <- "TimeProfileTest"
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # create new timerange tags
  dtTimeRange <- xlsxReadData(wb, sheetName = "TimeRange")
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
  dt[, timeRange_h0_6 := as.character(NA)]
  dt[, timeRange_h0_6 := NA]
  dt[, timeRange_h6_24 := as.character(NA)]
  dt[, timeRange_h6_24 := NA]


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

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)


  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
# Cleanup function to remove the electronic package folder
cleanupElectronicPackage <- function(projectConfiguration) {
  packageFolder <- suppressWarnings(projectConfiguration$addOns$electronicPackageFolder)
  if (dir.exists(packageFolder)) {
    unlink(packageFolder, recursive = TRUE) # Remove the folder and its contents
  }
}
# function to create workflowRmds to test TLF workflow export
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
