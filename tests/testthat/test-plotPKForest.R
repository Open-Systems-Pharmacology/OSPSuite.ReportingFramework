# testProject was set up by setup.R
pkParameterDT <- loadPKParameter(
  projectConfiguration = projectConfiguration,
  scenarioList = scenarioList
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
        pkParameterObserved = dataObservedPK
      )
    ))

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "abs_data_pediatric_AUC_inf_linear",
    fig = plotList[["pediatric-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "abs_data_pediatric_F_tEnd_log",
    fig = plotList[["pediatric-F_tEnd-log"]]
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
    fig = plotList[["pediatric-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_pediatric_F_tEnd_log",
    fig = plotList[["pediatric-F_tEnd-log"]]
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
    fig = plotList[["crossover-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_F_tEnd_log",
    fig = plotList[["pediatric-F_tEnd-log"]]
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
        pkParameterObserved = dataObservedPK
      )
    )

  expect_equal(length(plotList), 6)

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_data_AUC_inf_linear",
    fig = plotList[["crossover-AUC_inf-linear"]]
  )

  vdiffr::expect_doppelganger(
    title = "PE_Ratio_crossover_data_F_tEnd_log",
    fig = plotList[["pediatric-F_tEnd-log"]]
  )

  rm(plotList)
})
