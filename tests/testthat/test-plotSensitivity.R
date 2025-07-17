# testProject was set up by setup.R

mockManualEditingsPlotSensitivityTest(
  projectConfiguration = projectConfiguration,
  sensitivityScenario = names(scenarioListInd)[2],
  sensitivitySheet = "smallSelection"
)

test_that("sensitivity plots", {
  plotList <-
    runPlot(
      nameOfplotFunction = "plotSensitivity",
      configTableSheet = "SensitivityPlots",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      inputs = list(
        scenarioList = scenarioListInd
      )
    )

  expect_equal(length(plotList), 6)
  expect_equal(nrow(plotList[["sensitivity_all-A"]]), 4)
  expect_contains(plotList[["sensitivity_all-A"]]$Parameter, expected = "DrugX Fraction unbound")

  vdiffr::expect_doppelganger(
    title = "sensitivity_90",
    fig = plotList$sensitivity_90
  )

  rm(plotList)
})
