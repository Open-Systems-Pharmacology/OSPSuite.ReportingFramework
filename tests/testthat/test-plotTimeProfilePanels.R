# testProject was set up by setup.R
dataObserved <- readObservedDataByDictionary(projectConfiguration)
dataObserved <- rbind(dataObserved,
  aggregateObservedDataGroups(
    dataObserved = dataObserved,
    groups = "1234_adults_iv"
  ),
  aggregateObservedDataGroups(
    dataObserved = dataObserved,
    groups = "1234_adults_po",
    aggregationFlag = "Percentile"
  ),
  fill = TRUE
)


test_that("Default Config For Histograms", {
  addDefaultConfigForTimeProfilePlots(
    projectConfiguration = projectConfiguration,
    sheetName = "TimeProfileTest",
    dataObserved = dataObserved,
    overwrite = TRUE
  )

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  expect_contains(wb$sheet_names, "TimeProfileTest")
})

# add configuration for testcases
mockManualEditingsPlotTimeProfileTest(projectConfiguration = projectConfiguration)

test_that("Time profiles of individual scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    c(runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c("Individuals_withData"),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved
      )
    ),
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c( "Individuals_withoutData"),
      inputs = list(
        scenarioResults = scenarioResultsInd
      )
    ))

  expect_equal(length(plotList), 2)

  for (pName in names(plotList)) {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

  rm(plotList)
})


test_that("Predicted vs observed of individual scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c("Individuals_withData_pvo"),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved
      )
    )

  for (pName in names(plotList)) {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

  rm(plotList)
})


test_that("Time profiles of virtual twin scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c(
        "VirtualTwin",
        "VirtualTwin_withData_all",
        "VirtualTwin_withData_selected",
        "VirtualTwin_withReferenceInd"
      ),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved
      )
    )

  expect_equal(length(plotList), 4)

  for (pName in names(plotList)) {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }


  expect_error(
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = "VirtualTwin_withReferencePop",
      inputs = list(
        scenarioResults = c(
          scenarioResults,
          scenarioResultsInd
        ),
        dataObserved = dataObserved
      )
    )
  )

  rm(plotList)
})

test_that("Predicted vs observed of virtual twin scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c("VirtualTwin_withData_selected_pvo"),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved
      )
    )

  for (pName in names(plotList)) {
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }
  rm(plotList)
})

test_that("Time profiles with populations", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c(
        "Pop_withoutData",
        "Pop_withIndividualData",
        "Pop_withAggregatedData",
        "Pop_withReference"
      ),
      inputs = list(
        scenarioResults = scenarioResults,
        dataObserved = dataObserved
      )
    )


  for (pName in names(plotList)) {
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

  expect_error(runPlot(
    nameOfplotFunction = "plotTimeProfiles",
    configTableSheet = "TimeProfileTest",
    projectConfiguration = projectConfiguration,
    suppressExport = TRUE,
    plotNames = c("Pop_withReference_2"),
    inputs = list(
      scenarioResults = scenarioResults,
      dataObserved = dataObserved
    )
  ))

  expect_error(runPlot(
    nameOfplotFunction = "plotTimeProfiles",
    configTableSheet = "TimeProfileTest",
    projectConfiguration = projectConfiguration,
    suppressExport = TRUE,
    plotNames = c("Pop_withAggregatedData_Percentiles"),
    inputs = list(
      scenarioResults = scenarioResults,
      dataObserved = dataObserved
    )
  ))

  rm(plotList)
})


test_that("Predicted vs observed of populations", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = "Pop_withAggregatedData_pvo",
      inputs = list(
        scenarioResults = scenarioResults,
        dataObserved = dataObserved
      )
    )

  for (pName in names(plotList)) {
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }
  rm(plotList)
})

test_that("Time profiles vs time range", {
  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c("Pop_withTimeRanges"),
      inputs = list(
        scenarioResults = scenarioResults,
        dataObserved = dataObserved
      )
    )

  for (pName in names(plotList)) {
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }
  rm(plotList)
})

test_that("Predicted vs observed vs time range", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      suppressExport = TRUE,
      plotNames = c("Pop_withTimeRanges_pvo"),
      inputs = list(
        scenarioResults = scenarioResults,
        dataObserved = dataObserved
      )
    )

  for (pName in names(plotList)) {
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }
  rm(plotList)
})

test_that("QC functionality", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # case 1 returns unused individuals
  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      plotNames = c("Individuals_withData"),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved[group == "1234_adults_iv"],
        checkForUnusedData = TRUE
      )
    )

  unusedSubjects <- plotList$unusedDataRows$subjectId %>% unique()
  expect_length(unusedSubjects, n = 3)
  expect_contains(unusedSubjects, expected = "50")

  # case 2 returns nothing
  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      plotNames = c("Individuals_withData"),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved[group == "1234_adults_iv" &
          subjectId %in% c("13", "30", "41")],
        checkForUnusedData = TRUE
      )
    )

  unusedSubjects <- plotList$unusedDataRows$subjectId %>% unique()
  expect_equal(nrow(plotList$unusedDataRows), expected = 0)

  plotList <-
    runPlot(
      nameOfplotFunction = "plotTimeProfiles",
      configTableSheet = "TimeProfileTest",
      projectConfiguration = projectConfiguration,
      plotNames = c("Individuals_withData"),
      inputs = list(
        scenarioResults = scenarioResultsInd,
        dataObserved = dataObserved[group == "1234_adults_iv" &
          subjectId %in% c("13", "30", "41")],
        checkForUnusedData = TRUE
      )
    )

  rm(plotList)
})
