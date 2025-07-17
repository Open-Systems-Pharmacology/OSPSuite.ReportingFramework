# testProject was set up by setup.R
pkParameterDT <- loadPKParameter(
  projectConfiguration = projectConfiguration,
  scenarioList = scenarioList
)


test_that("Default Config For Boxplots", {
  addDefaultConfigForPKBoxwhsikerPlots(
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT,
    sheetName = "PKParameter_BoxplotTest",
    overwrite = TRUE
  )

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  expect_contains(wb$sheet_names, "PKParameter_BoxplotTest")
  dt <- xlsxReadData(wb = wb, sheetName = "PKParameter_BoxplotTest", skipDescriptionRow = TRUE)
})

# prepare configtable
mockManualEditingsPlotBoxwhsikerTest(projectConfiguration)

test_that("Boxwhsiker crossover ", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotPKBoxwhisker",
      suppressExport = TRUE,
      plotNames = "crossover",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_BoxplotTest",
      inputs = list(
        pkParameterDT = pkParameterDT,
        percentiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
      )
    )

  expect_equal(length(plotList), 4)

  expect_contains(names(plotList$`crossover_AUC_inf-ratio`), expected = "2.5th percentile")

  vdiffr::expect_doppelganger(
    title = "crossover_AUC_inf",
    fig = plotList[["crossover_AUC_inf-log-ratio"]]
  )

  rm(plotList)
})


test_that("Boxwhsiker pediatrics ", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotPKBoxwhisker",
      suppressExport = TRUE,
      plotNames = "pediatric",
      projectConfiguration = projectConfiguration,
      configTableSheet = "PKParameter_BoxplotTest",
      inputs = list(
        pkParameterDT = pkParameterDT,
        xAxisTextAngle = 30,
        colorVector = c(pediatric = "green", adult = "grey"),
        facetAspectRatio = 0.3
      )
    )

  expect_equal(length(plotList), 4)

  expect_equal(nrow(plotList[["pediatric-abs-A"]]), 5)
  expect_equal(ncol(plotList[["pediatric-abs-A"]]), 14)

  vdiffr::expect_doppelganger(
    title = "pediatric-linear-abs",
    fig = plotList[["pediatric-linear-abs"]]
  )

  rm(plotList)
})
