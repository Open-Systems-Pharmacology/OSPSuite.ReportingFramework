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
    c(
      runPlot(
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
        plotNames = c("Individuals_withoutData"),
        inputs = list(
          scenarioResults = scenarioResultsInd,
          y2ScaleArgs = list(limits = c(0, 1.05))
        )
      )
    )

  expect_equal(length(plotList), 2)

  expectDoppelgangerLoop(plotList)

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

  expectDoppelgangerLoop(plotList)

  rm(plotList)
})


test_that("Time profiles of virtual twin scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  expect_error(runPlot(
    nameOfplotFunction = "plotTimeProfiles",
    configTableSheet = "TimeProfileTest",
    projectConfiguration = projectConfiguration,
    suppressExport = TRUE,
    plotNames = c(
      "VirtualTwin_withReferencePop"
    ),
    inputs = list(
      scenarioResults = c(
        scenarioResults,
        scenarioResultsInd
      ),
      dataObserved = dataObserved
    )
  ))


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
        "VirtualTwin_withReferenceInd",
        "VirtualTwin_withReferenceTwinPop"
      ),
      inputs = list(
        scenarioResults = c(
          scenarioResults,
          scenarioResultsInd
        ),
        dataObserved = dataObserved
      )
    )

  expect_equal(length(plotList), 5)

  expectDoppelgangerLoop(plotList)

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

  expectDoppelgangerLoop(plotList)

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

  expectDoppelgangerLoop(plotList)

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

  expectDoppelgangerLoop(plotList)

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

  expectDoppelgangerLoop(plotList)

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

  expectDoppelgangerLoop(plotList)

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

  unusedSubjects <- plotList$unusedDataRows$subjectId |> unique()
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

  unusedSubjects <- plotList$unusedDataRows$subjectId |> unique()
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

# Unit tests for internal functions
test_that(".mergeDataGroups merges data groups correctly", {
  # Create test data
  dtNewConfig <- data.table(
    scenario = c("scenario1", "scenario2", "scenario3"),
    plotName = c("plot1", "plot2", "plot3")
  )

  dtDataGroups <- data.table(
    defaultScenario = c("scenario1", "scenario1", "scenario2"),
    group = c("group1", "group2", "group3")
  )

  # Test without observed data
  result <- ospsuite.reportingframework:::.mergeDataGroups(
    dtNewConfig = dtNewConfig,
    dtDataGroups = dtDataGroups,
    dataObserved = NULL
  )

  expect_s3_class(result, "data.table")
  expect_true("dataGroupIds" %in% names(result))
  expect_true("individualIds" %in% names(result))

  # Check that dataGroupIds are correctly merged
  expect_equal(result[scenario == "scenario1"]$dataGroupIds, "group1, group2")
  expect_equal(result[scenario == "scenario2"]$dataGroupIds, "group3")
  expect_true(is.na(result[scenario == "scenario3"]$dataGroupIds))

  # Check that individualIds is set to "*" where dataGroupIds is not NA
  expect_equal(result[scenario == "scenario1"]$individualIds, "*")
  expect_equal(result[scenario == "scenario2"]$individualIds, "*")
  expect_true(is.na(result[scenario == "scenario3"]$individualIds))
})

test_that(".mergeDataGroups handles observed data correctly", {
  # Create test data
  dtNewConfig <- data.table(
    scenario = c("scenario1", "scenario2"),
    plotName = c("plot1", "plot2")
  )

  dtDataGroups <- data.table(
    defaultScenario = c("scenario1", "scenario1"),
    group = c("group1", "group2")
  )

  dataObserved <- data.table(
    group = c("group1", "group1", "group2", "group2"),
    outputPathId = c("output1", "output2", "output2", "output3")
  )

  # Test with observed data
  result <- ospsuite.reportingframework:::.mergeDataGroups(
    dtNewConfig = dtNewConfig,
    dtDataGroups = dtDataGroups,
    dataObserved = dataObserved
  )

  expect_s3_class(result, "data.table")
  expect_true("outputPathIds" %in% names(result))

  # Check that outputPathIds are correctly populated from observed data
  outputPaths <- result[scenario == "scenario1"]$outputPathIds
  expect_true(grepl("output1", outputPaths))
  expect_true(grepl("output2", outputPaths))
  expect_true(grepl("output3", outputPaths))
})

test_that(".mergeDataGroups handles empty data groups", {
  # Create test data with no matching scenarios
  dtNewConfig <- data.table(
    scenario = c("scenario1", "scenario2"),
    plotName = c("plot1", "plot2")
  )

  dtDataGroups <- data.table(
    defaultScenario = character(0),
    group = character(0)
  )

  result <- ospsuite.reportingframework:::.mergeDataGroups(
    dtNewConfig = dtNewConfig,
    dtDataGroups = dtDataGroups,
    dataObserved = NULL
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  # All dataGroupIds should be NA when no groups match
  expect_true(all(is.na(result$dataGroupIds)))
})
