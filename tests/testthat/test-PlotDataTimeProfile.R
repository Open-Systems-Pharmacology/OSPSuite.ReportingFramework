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

timeTags <- factor(
  c("Tag1", "Tag2"),
  levels = c("Tag1", "Tag2"),
  ordered = TRUE
)

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
  result <- splitCaptionByIndividuals(
    configTable,
    individualIdVector = c("1", "2"),
    dtCaption
  )
  expect_true("individualId" %in% names(result))
  expect_equal(nrow(result), 8) # Adjust based on expected output
})


test_that("addTimeTagsToCaption works correctly", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- addTimeTagsToCaption(
    dtCaption,
    timeTags,
    splitPlotsPerTimeRange = 0
  )
  expect_true("timeRangeTag" %in% names(result))
  expect_contains(result$timeRangeTag, timeTags)
  expect_equal(nrow(result), length(timeTags) * nrow(dtCaption)) # Check number of rows after adding tags
})

test_that("setTimeRangeFilter works correctly", {
  result <- setTimeRangeFilter(splitPlotsPerTimeRange = 1, timeTags)
  expect_equal(length(result), 2) # Check length of filters
})

test_that("restructureApplicationTimeByScenarioIndex works correctly", {
  applicationTimes <- list(A = c(1, 2), B = c(3, 4))
  result <- restructureApplicationTimeByScenarioIndex(
    applicationTimes,
    configTable
  )
  expect_equal(length(result), 4) # Check number of scenarios
})

test_that("getOutputPathsPerScenario works correctly", {
  result <- getOutputPathsPerScenario(configTable, dtOutputPaths)
  expect_true(is.list(result))
  expect_equal(length(result), 2) # Check number of scenarios
})

test_that("getObservedUnitConversionDT works correctly", {
  dataObserved <- data.table(
    outputPathId = c("Path1", "Path2"),
    yUnit = c("g", "kg")
  )
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

# Edge case tests for getPlotIdForColumns
test_that("getPlotIdForColumns handles empty string column values", {
  emptyConfig <- data.table(
    scenario = "",
    outputPathIds = ""
  )
  result <- getPlotIdForColumns(emptyConfig, "outputPathIds")
  expect_true("plotId" %in% names(result))
  expect_true(nrow(result) >= 1)
})

test_that("getPlotIdForColumns handles special characters in column values", {
  specialConfig <- data.table(
    scenario = "A & B",
    outputPathIds = "Path|1,2;3"
  )
  result <- getPlotIdForColumns(specialConfig, "outputPathIds")
  expect_true("plotId" %in% names(result))
})

# Edge case tests for splitCaptionByIndividuals
test_that("splitCaptionByIndividuals handles empty individual vector", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- splitCaptionByIndividuals(
    configTable,
    individualIdVector = character(0),
    dtCaption
  )
  expect_true("individualId" %in% names(result))
})

test_that("splitCaptionByIndividuals preserves all data with single individual", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- splitCaptionByIndividuals(
    configTable,
    individualIdVector = "1",
    dtCaption
  )
  expect_true("individualId" %in% names(result))
  expect_equal(result$individualId[1], "1")
})

# Edge case tests for addTimeTagsToCaption
test_that("addTimeTagsToCaption handles empty timeTags", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  emptyTags <- factor(character(0), ordered = TRUE)
  result <- addTimeTagsToCaption(
    dtCaption,
    emptyTags,
    splitPlotsPerTimeRange = 0
  )
  expect_true(is.data.table(result))
})

test_that("addTimeTagsToCaption handles splitPlotsPerTimeRange = 1", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- addTimeTagsToCaption(
    dtCaption,
    timeTags,
    splitPlotsPerTimeRange = 1
  )
  expect_true("timeRangeTag" %in% names(result))
  expect_true(nrow(result) >= nrow(dtCaption))
})

# Edge case tests for setTimeRangeFilter
test_that("setTimeRangeFilter handles empty timeTags", {
  emptyTags <- factor(character(0), ordered = TRUE)
  result <- setTimeRangeFilter(splitPlotsPerTimeRange = 1, emptyTags)
  expect_true(is.list(result) || length(result) == 0)
})

test_that("setTimeRangeFilter handles single timeTag", {
  singleTag <- factor("SingleTag", ordered = TRUE)
  result <- setTimeRangeFilter(splitPlotsPerTimeRange = 0, singleTag)
  expect_true(is.list(result))
})

# Edge case tests for restructureApplicationTimeByScenarioIndex
test_that("restructureApplicationTimeByScenarioIndex handles empty applicationTimes", {
  result <- restructureApplicationTimeByScenarioIndex(list(), configTable)
  expect_true(is.list(result) || length(result) == 0)
})

test_that("restructureApplicationTimeByScenarioIndex handles mismatched scenarios", {
  mismatchedTimes <- list(C = c(1, 2), D = c(3, 4))
  result <- restructureApplicationTimeByScenarioIndex(
    mismatchedTimes,
    configTable
  )
  expect_true(is.list(result))
})

# Edge case tests for getOutputPathsPerScenario
test_that("getOutputPathsPerScenario handles missing output paths", {
  emptyPaths <- data.table(
    outputPathId = character(0),
    OutputPaths = character(0)
  )
  result <- getOutputPathsPerScenario(configTable, emptyPaths)
  expect_true(is.list(result))
})

test_that("getOutputPathsPerScenario with single path", {
  singlePath <- data.table(
    outputPathId = "Path1",
    OutputPaths = "model|Path1"
  )
  result <- getOutputPathsPerScenario(configTable, singlePath)
  expect_true(is.list(result))
})

# Edge case tests for getObservedUnitConversionDT
test_that("getObservedUnitConversionDT handles multiple rows", {
  dataObserved <- data.table(
    outputPathId = c("Path1", "Path2"),
    yUnit = c("g", "kg")
  )
  dtUnit <- data.table(
    outputPathId = c("Path1", "Path2"),
    dimension = c("Mass", "Mass"),
    yUnit = c("kg", "kg"),
    displayUnit = c("g", "kg"),
    unitFactor = c(1000, 1)
  )
  result <- getObservedUnitConversionDT(dataObserved, dtUnit)
  expect_true(is.data.table(result))
  expect_equal(nrow(result), 2)
})
