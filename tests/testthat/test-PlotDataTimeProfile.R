# Example data for testing
configTable <- data.table(
  scenario = c("A", "A", "B", "B"),
  scenarioIndex = c(1, 2, 3, 4),
  individualIds = c("1,2", "3", "*", "4"),
  outputPathIds = c("Path1", "Path2", "Path1,Path2", "Path3"),
  timeUnit = c("day(s)", "day(s)", "day(s)", "day(s)"),
  timeOffset_Reference = c(0, 1, 2, 3),
  timeOffset = c(1, 1, 1, 1),
  timeRange_Tag1 = c("firstApplication", rep(NA, 3)),
  timeRange_2 = rep("total", 4),
  referenceScenario = c(NA, NA, NA, NA)
)

timeTags <- factor(c("Tag1", "Tag2"), levels = c("Tag1", "Tag2"), ordered = TRUE)

dtOutputPaths <- data.table(
  outputPathId = c("Path1", "Path2", "Path3"),
  OutputPaths = c("model|Path1", "model|Path2", "model|Path3")
)


# Unit tests
test_that("getPlotIdForColumns works correctly", {
  result <- getPlotIdForColumns(configTable, "outputPathIds")
  expect_true("plotId" %in% names(result))
  expect_equal(nrow(result), 5) # Check the number of rows
})

test_that("splitCaptionByIndividuals works correctly", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- splitCaptionByIndividuals(configTable, individualIdVector = c("1", "2"), dtCaption)
  expect_true("individualId" %in% names(result))
  expect_equal(nrow(result), 8) # Adjust based on expected output
})

test_that("determineFacetColumns works correctly", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- determineFacetColumns(dtCaption, 2, "vsOutput", "TestPlot")
  expect_equal(result, 2) # Adjust based on expected output
})

test_that("addTimeTagsToCaption works correctly", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- addTimeTagsToCaption(dtCaption, timeTags, "vsTimeRange")
  expect_true("timeRangeTag" %in% names(result))
  expect_contains(result$timeRangeTag, timeTags)
  expect_equal(nrow(result), length(timeTags) * nrow(dtCaption)) # Check number of rows after adding tags
})

test_that("setTimeRangeFilter works correctly", {
  result <- setTimeRangeFilter("vsTimeRange", timeTags)
  expect_equal(length(result), 2) # Check length of filters
})

test_that("restructureApplicationTimeByScenarioIndex works correctly", {
  applicationTimes <- list(A = c(1, 2), B = c(3, 4))
  result <- restructureApplicationTimeByScenarioIndex(applicationTimes, configTable)
  expect_equal(length(result), 4) # Check number of scenarios
})

test_that("getOutputPathsPerScenario works correctly", {
  result <- getOutputPathsPerScenario(configTable, dtOutputPaths)
  expect_true(is.list(result))
  expect_equal(length(result), 2) # Check number of scenarios
})

test_that("getObservedUnitConversionDT works correctly", {
  dataObserved <- data.table(outputPathId = c("Path1", "Path2"), yUnit = c("g", "kg"))
  dtUnit <-
    data.table(
      outputPathId = c("Path1", "Path2"),
      dimension = c("Mass", "Mass"),
      yUnit = c("kg", "kg"),
      displayUnit = c("g", "kg"),
      unitFactor = c(1000, 1)
    )

  result <- getObservedUnitConversionDT(dataObserved, dtUnit)
  expect_true("unitFactor" %in% names(result))
  expect_equal(nrow(result), 2) # Check number of rows
})

test_that("addTimeRangeTagsToData works correctly", {
  timeRangeColumns <- c("timeRange_Tag1")
  observedData <- randomObservedData()
  result <- addTimeRangeTagsToData(timeRangeColumns,
    dataOld = observedData, configTable,
    applicationTimes = list(list(startOfFirstApplication = 0, endOfFirstApplication = 10))
  )
  expect_true("timeRangeTag" %in% names(result))
  expect_equal(nrow(result), sum(observedData$xValues <= 10)) # Check number of rows
})
