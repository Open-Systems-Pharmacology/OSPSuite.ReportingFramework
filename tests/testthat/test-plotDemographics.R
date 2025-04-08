# testProject was set up by setup.R
pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                 scenarioList = scenarioList)

test_that("Default Config For Histograms", {
  addDefaultConfigForHistograms(projectConfiguration = projectConfiguration,
                                sheetName = "HistogramTest",
                                pkParameterDT = pkParameterDT,
                                overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  expect_contains(wb$sheet_names,'HistogramTest')
})

test_that("Default Config For Histograms", {
  addDefaultConfigForDistributionsVsDemographics(projectConfiguration = projectConfiguration,
                                sheetName = "RangePlotTest",
                                pkParameterDT = pkParameterDT,
                                overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  expect_contains(wb$sheet_names,'RangePlotTest')
})

# prepare configtable
mockManualEditingsPlotDemographicsTest(projectConfiguration)

test_that("PK histograms plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # createPlots
  plotList <- runPlot(
    nameOfplotFunction = "plotHistograms",
    projectConfiguration = projectConfiguration,
    configTableSheet = "HistogramTest",
    suppressExport = TRUE,
    plotNames = 'pkparameter2',
    inputs = list(scenarioList = scenarioList,
                  pkParameterDT = pkParameterDT,
                  colorVector = c( 'IV application' = 'red',
                                   'PO application' = 'green'),
                  plotAsFrequency = TRUE)
  )

  expect_equal(length(plotList),2)

  vdiffr::expect_doppelganger(
    title = "pkhistograms_F_tEnd_linear",
    fig = plotList$pkparameter2_F_tEnd_linear
  )
})


test_that("demographic histograms plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # createPlots
  plotList <- runPlot(
    nameOfplotFunction = "plotHistograms",
    projectConfiguration = projectConfiguration,
    configTableSheet = "HistogramTest",
    suppressExport = TRUE,
    plotNames = 'demographics',
    inputs = list(scenarioList = scenarioList,
                  pkParameterDT = pkParameterDT,
                  colorVector = c(adults = 'grey'))
  )

  expect_equal(length(plotList),2)

  vdiffr::expect_doppelganger(
    title = "dmhistograms_gender_linear",
    fig = plotList$demographics_gender_linear
  )
})

test_that("demographic range plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # createPlots
  plotList <- runPlot(
    nameOfplotFunction = "plotDistributionVsDemographics",
    projectConfiguration = projectConfiguration,
    configTableSheet = "RangePlotTest",
    suppressExport = TRUE,
    plotNames = 'demographics',
    inputs = list(scenarioList = scenarioList,
                  asStepPlot = FALSE,
                  facetAspectRatio = 0.3,
                  colorVector = c(pediatrics = ospsuite.plots::colorMaps[[1]][[1]],
                                  adults = 'grey'))
  )

  expect_equal(length(plotList),6)
  expect_equal(nrow(plotList[['demographics_weight']]),19)

  vdiffr::expect_doppelganger(
    title = "weight_linear",
    fig = plotList$demographics_weight_linear
  )
})

test_that("PK range plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <- runPlot(
    nameOfplotFunction = "plotDistributionVsDemographics",
    projectConfiguration = projectConfiguration,
    configTableSheet = "RangePlotTest",
    suppressExport = TRUE,
    plotNames = 'pkparameter2',
    inputs = list(scenarioList = scenarioList,
                  pkParameterDT = pkParameterDT,
                  asStepPlot = TRUE,
                  aggregationFlag = "GeometricStdDev",
                  colorVector = c(pediatrics = ospsuite.plots::colorMaps[[1]][[1]],
                                  adults = 'grey'))
  )

  expect_equal(length(plotList),4)

  expect_equal(nrow(plotList$pkparameter2_F_tEnd_A),21)

  vdiffr::expect_doppelganger(
    title = "F_tEnd_log",
    fig = plotList$pkparameter2_F_tEnd_log
  )
})








