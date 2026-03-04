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
    pkParameter = unique(dataObservedPK$pkParameter)[1],
    dataGroupId = unique(dataObservedPK$group)[1]
  )

  result <- ospsuite.reportingframework:::.filterParameterObserved(
    dataObservedPK = dataObservedPK,
    onePlotConfig = onePlotConfig
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that(".filterParameterSimulated filters simulated data correctly", {
  skip_if(nrow(pkParameterDT) == 0, "No PK parameter data available")

  onePlotConfig <- data.table(
    scenario = unique(pkParameterDT$scenario)[1],
    outputPathId = unique(pkParameterDT$outputPathId)[1],
    pkParameter = unique(pkParameterDT$pkParameter)[1],
    referenceScenario = NA
  )

  loadConfigTableEnvironment(projectConfiguration)

  result <- ospsuite.reportingframework:::.filterParameterSimulated(
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT,
    onePlotConfig = onePlotConfig,
    ratioMode = FALSE,
    coefficientOfVariation = FALSE,
    asPointeEstimate = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that(".adjustForestDataPerGroup adjusts data correctly", {
  skip_if(nrow(pkParameterDT) == 0, "No PK parameter data available")

  testData <- pkParameterDT[c(1,1001,2001,4000)]
  testData$group <- "TestGroup"
  testData[, scenarioShortName :=  scenario]
  testData$scenarioGroup <-  "scenarioGroup"
  testData$dataType <- 'observed'

  onePlotConfig <- data.table(
    scenario = unique(testData$scenario)[1],
    xScale = "linear"
  )

  result <- ospsuite.reportingframework:::.adjustForestDataPerGroup(
    dataGroup = testData,
    onePlotConfig = onePlotConfig
  )

  expect_s3_class(result, "data.table")
  expect_contains(names(result),'plotTag')
})


test_that(".checkPrecision checks precision requirements", {
  testData <- data.table(
    xValues = c(1.0, 2.0, 3.0),
    xMin = c(0.95, 1.9, 2.8),
    xMax = c(1.05, 2.1, 3.2)
  )

  result <- ospsuite.reportingframework:::.checkPrecision(testData)

  expect_s3_class(result, "data.table")
  expect_true("precision" %in% names(result))
})


test_that(".getTableLabelsForPKForest generates table labels", {
  testData <- data.table(
    group = c("Group1", "Group2"),
    xValues = c(1.5, 2.5),
    xMin = c(1.0, 2.0),
    xMax = c(2.0, 3.0),
    numberOfIndividuals = c(10, 15),
    xErrorType = rep('mean|xMin|xMax',2)
  )

  result <- ospsuite.reportingframework:::.getTableLabelsForPKForest(testData)

  expect_length(result, 3)
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

  expect_s3_class(result, "gg")
})

test_that(".getColumnSelectionForPKForest selects columns correctly", {
  testData <- data.table(
    displayNamePKParameter = c('AUC','AUC','Cmax','Cmax'),
    displayUnitPKParameter = c('µg/L*h','µg/L*h','µg/L','µg/L*h'),
    scenarioShortName = c('scenario1','scenario1','scenario2','scenario2'),
    scenarioGroup = c('Group','Group')
  )

  # Test for ratios (ratioMode != 'none') and more the one PK Parameter
  result <- ospsuite.reportingframework:::.getColumnSelectionForPKForest(
    plotData = testData,
    ratioMode = 'other'
  )

  expect_type(result, "list")
  expect_contains( names(result), c('yColumn','yFacetColumns','xLabel'))
  expect_equal(result$xLabel,'Ratio')
  expect_length(result$yFacetColumns,2)

  # Test for ratios (ratioMode != 'none') and only one PK Parameter
  result <- ospsuite.reportingframework:::.getColumnSelectionForPKForest(
    plotData = testData[c(1,2)],
    ratioMode = 'other'
  )

  expect_type(result, "list")
  expect_contains( names(result), c('yColumn','yFacetColumns','xLabel'))
  expect_equal(result$xLabel,'AUC Ratio')
  expect_length(result$yFacetColumns,1)


  # Test for absolute values (ratioMode = 'none') and more than one  scenario
  result <- ospsuite.reportingframework:::.getColumnSelectionForPKForest(
    plotData = testData,
    ratioMode = 'none'
  )

  expect_type(result, "list")
  expect_contains( names(result), c('yColumn','yFacetColumns','xLabel'))
  expect_equal(result$xLabel,"AUC [µg/L*h]")
  expect_length(result$yFacetColumns,1)


  # Test for absolute values (ratioMode = 'none') and only one  scenario
  result <- ospsuite.reportingframework:::.getColumnSelectionForPKForest(
    plotData = testData[c(1,2),],
    ratioMode = 'none'
  )

  expect_type(result, "list")
  expect_contains( names(result), c('yColumn','yFacetColumns','xLabel'))
  expect_equal(result$xLabel,"scenario1 AUC [µg/L*h]")
  expect_length(result$yFacetColumns,0)

})

