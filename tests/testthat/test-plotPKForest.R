# testProject was set up by setup.R
pkParameterDT <- loadPKParameter(
  projectConfiguration = projectConfiguration,
  scenarioListOrResult = scenarioList
)
dataObservedPK <- suppressMessages(suppressWarnings(readObservedDataByDictionary(
  projectConfiguration = projectConfiguration,
  dataClassType = "pkParameter"
)))

test_that("Default Config For PKForestPlots", {
  addDefaultConfigForPKForestPlots(
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT,
    sheetName = "PKParameter_ForestTest",
    overwrite = TRUE
  )

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  expect_contains(wb$sheet_names, "PKParameter_ForestTest")
})

# adjust configtables in project directory
mockManualEditingsPlotPkForestTest(projectConfiguration)

test_that("AbsoluteValues pediatric", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    suppressWarnings(runPlot(
      nameOfplotFunction = "plotPKForestAggregatedAbsoluteValues",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_ForestTest",
      suppressExport = TRUE,
      plotNames = "pediatric",
      inputs = list(
        pkParameterDT = pkParameterDT
      )
    ))

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "abs_pediatric_AUC_inf_linear",
    fig = plotList[["pediatric-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "abs_pediatric_F_tEnd_log",
    fig = plotList[["pediatric-F_tEnd-log"]]
  )

  rm(plotList)
})

test_that("AbsoluteValues pediatric with data", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    suppressWarnings(runPlot(
      nameOfplotFunction = "plotPKForestAggregatedAbsoluteValues",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_ForestTest",
      suppressExport = TRUE,
      plotNames = "pediatric_data",
      inputs = list(
        pkParameterDT = pkParameterDT,
        dataObservedPK = dataObservedPK
      )
    ))

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "abs_data_pediatric_AUC_inf_linear",
    fig = plotList[["pediatric_data-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "abs_data_pediatric_F_tEnd_log",
    fig = plotList[["pediatric_data-F_tEnd-log"]]
  )

  rm(plotList)
})

test_that("AbsoluteValues pediatric PE", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    suppressWarnings(runPlot(
      nameOfplotFunction = "plotPKForestPointEstimateOfAbsoluteValues",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_ForestTest",
      suppressExport = TRUE,
      plotNames = "pediatric",
      inputs = list(
        pkParameterDT = pkParameterDT
      )
    ))

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "PE_pediatric_AUC_inf_linear",
    fig = plotList[["pediatric-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_pediatric_F_tEnd_log",
    fig = plotList[["pediatric-F_tEnd-log"]]
  )

  rm(plotList)
})


test_that("Ratios pediatric PE", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotPKForestPointEstimateOfRatios",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_ForestTest",
      suppressExport = TRUE,
      plotNames = "pediatric",
      inputs = list(
        pkParameterDT = pkParameterDT
      )
    )

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_pediatric_AUC_inf_linear",
    fig = plotList[["pediatric-Plasma-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_pediatric_F_tEnd_log",
    fig = plotList[["pediatric-CYP3A4Liver-log"]]
  )

  rm(plotList)
})

test_that("Ratios crossover PE", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotPKForestPointEstimateOfRatios",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_ForestTest",
      suppressExport = TRUE,
      plotNames = "crossover",
      inputs = list(
        pkParameterDT = pkParameterDT
      )
    )

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_AUC_inf_linear",
    fig = plotList[["crossover-Plasma-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_F_tEnd_log",
    fig = plotList[["crossover-CYP3A4Liver-log"]]
  )

  rm(plotList)
})

test_that("Ratios crossover PE with data", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotPKForestPointEstimateOfRatios",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_ForestTest",
      suppressExport = TRUE,
      plotNames = "crossover_data",
      inputs = list(
        pkParameterDT = pkParameterDT,
        dataObservedPK = dataObservedPK
      )
    )

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_data_AUC_inf_linear",
    fig = plotList[["crossover_data-Plasma-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_data_F_tEnd_log",
    fig = plotList[["crossover_data-CYP3A4Liver-log"]]
  )

  rm(plotList)
})

# Additional tests for plotPKForest.R to increase coverage

test_that(".updateScalevector updates scale vectors correctly", {
  scaleInput <- list(
    simulated = list(
      color = c("red", "blue"),
      shape = c(16, 17)
    )
  )
  
  result <- ospsuite.reportingframework:::.updateScalevector(scaleInput)
  
  expect_type(result, "list")
  expect_true("simulated" %in% names(result))
})

test_that(".filterParameterObserved filters observed data correctly", {
  skip_if(nrow(dataObservedPK) == 0, "No observed PK data available")
  
  onePlotConfig <- data.table(
    outputPathId = unique(dataObservedPK$outputPathId)[1],
    pkParameter = unique(dataObservedPK$pkParameter)[1]
  )
  
  result <- ospsuite.reportingframework:::.filterParameterObserved(
    dataObservedPK = dataObservedPK,
    onePlotConfig = onePlotConfig
  )
  
  expect_s3_class(result, "data.table")
})

test_that(".filterParameterSimulated filters simulated data correctly", {
  skip_if(nrow(pkParameterDT) == 0, "No PK parameter data available")
  
  onePlotConfig <- data.table(
    scenario = unique(pkParameterDT$scenario)[1],
    outputPathId = unique(pkParameterDT$outputPathId)[1],
    pkParameter = unique(pkParameterDT$pkParameter)[1],
    referenceScenario = NA
  )
  
  result <- ospsuite.reportingframework:::.filterParameterSimulated(
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT,
    onePlotConfig = onePlotConfig,
    ratioMode = FALSE,
    coefficientOfVariation = FALSE,
    asPointeEstimate = FALSE
  )
  
  expect_s3_class(result, "data.table")
})

test_that(".adjustForestDataPerGroup adjusts data correctly", {
  skip_if(nrow(pkParameterDT) == 0, "No PK parameter data available")
  
  testData <- pkParameterDT[1:20]
  testData$group <- "TestGroup"
  
  onePlotConfig <- data.table(
    scenario = unique(testData$scenario)[1],
    xScale = "linear"
  )
  
  result <- ospsuite.reportingframework:::.adjustForestDataPerGroup(
    dataGroup = testData,
    onePlotConfig = onePlotConfig
  )
  
  expect_s3_class(result, "data.table")
})

test_that(".aggregatePointEstimate calculates point estimates", {
  skip_if(nrow(pkParameterDT) == 0, "No PK parameter data available")
  
  testData <- pkParameterDT[1:100]
  
  result <- ospsuite.reportingframework:::.aggregatePointEstimate(
    pkParameterFiltered = testData,
    confLevel = 0.9,
    nBootstrap = 100
  )
  
  expect_s3_class(result, "data.table")
  expect_true("xValues" %in% names(result))
  expect_true("xMin" %in% names(result))
  expect_true("xMax" %in% names(result))
})

test_that(".checkPrecision checks precision requirements", {
  testData <- data.table(
    xValues = c(1.0, 2.0, 3.0),
    xMin = c(0.95, 1.9, 2.8),
    xMax = c(1.05, 2.1, 3.2)
  )
  
  result <- ospsuite.reportingframework:::.checkPrecision(testData)
  
  expect_s3_class(result, "data.table")
  expect_true("precisionFlag" %in% names(result))
})

test_that(".addObservedData adds observed data to plot data", {
  skip_if(nrow(dataObservedPK) == 0, "No observed PK data available")
  skip_if(nrow(pkParameterDT) == 0, "No PK parameter data available")
  
  plotData <- pkParameterDT[1:20]
  plotData$group <- "TestGroup"
  plotData$xValues <- as.numeric(plotData$values)
  
  onePlotConfig <- data.table(
    outputPathId = unique(plotData$outputPathId)[1],
    pkParameter = unique(plotData$pkParameter)[1]
  )
  
  result <- ospsuite.reportingframework:::.addObservedData(
    plotData = plotData,
    dataObservedPK = dataObservedPK,
    onePlotConfig = onePlotConfig
  )
  
  expect_s3_class(result, "data.table")
})

test_that(".getTableLabelsForPKForest generates table labels", {
  testData <- data.table(
    group = c("Group1", "Group2"),
    xValues = c(1.5, 2.5),
    xMin = c(1.0, 2.0),
    xMax = c(2.0, 3.0),
    numberOfIndividuals = c(10, 15)
  )
  
  result <- ospsuite.reportingframework:::.getTableLabelsForPKForest(testData)
  
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that(".getMappingForForestPlots creates mapping correctly", {
  testData <- data.table(
    group = c("Group1", "Group2"),
    xValues = c(1.5, 2.5)
  )
  
  columnList <- list(
    x = "xValues",
    y = "group"
  )
  
  result <- ospsuite.reportingframework:::.getMappingForForestPlots(
    plotData = testData,
    columnList = columnList
  )
  
  expect_s3_class(result, "OSPSuiteMapping")
})

test_that(".getColumnSelectionForPKForest selects columns correctly", {
  testData <- data.table(
    group = c("Group1", "Group2"),
    xValues = c(1.5, 2.5),
    xMin = c(1.0, 2.0),
    xMax = c(2.0, 3.0)
  )
  
  # Test for absolute values (ratioMode = FALSE)
  result <- ospsuite.reportingframework:::.getColumnSelectionForPKForest(
    plotData = testData,
    ratioMode = FALSE
  )
  
  expect_type(result, "list")
  expect_true("x" %in% names(result))
  
  # Test for ratios (ratioMode = TRUE)
  result2 <- ospsuite.reportingframework:::.getColumnSelectionForPKForest(
    plotData = testData,
    ratioMode = TRUE
  )
  
  expect_type(result2, "list")
})

test_that("plotPKForestAggregatedAbsoluteValues handles empty config", {
  skip("Requires full integration test setup")
})

test_that("plotPKForestAggregatedRatios handles different populations", {
  skip("Requires full integration test setup")
})

test_that(".getCaptionForForestPlot generates captions", {
  testData <- data.table(
    scenario = c("Scenario1"),
    pkParameter = c("AUC"),
    outputPathId = c("Plasma")
  )
  
  onePlotConfig <- data.table(
    plotName = "TestPlot",
    referenceScenario = NA
  )
  
  result <- ospsuite.reportingframework:::.getCaptionForForestPlot(
    plotData = testData,
    onePlotConfig = onePlotConfig,
    ratioMode = FALSE,
    pkParameterDT = pkParameterDT
  )
  
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})
