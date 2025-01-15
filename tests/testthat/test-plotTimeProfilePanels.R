projectConfiguration <- suppressMessages(setUpTestProject())

# Create example data for testing
configTablePlots <- data.table(
  plotName = c("Plot1", "Plot1", "Plot2", "Plot2"),
  column1 = c("A", "A", "B", "B"),
  column2 = c("X", "X", "X", "Y"),
  outputPathIds = c("id1", "id2", "id3", "id4")
)

test_that("validateGroupConsistency function test", {
  # Test if the function correctly checks for unique values of panel columns for each PlotName
  expect_error(validateGroupConsistency(configTablePlots, c("column1", "column2")))

})



# Create example data for testing
configTablePlots <- data.table(
  value1 = c(1, 2, 3, 4),
  value2 = c("A", "B", "C", "D"),
  timeRange_Valid1 = c(NA, "total", "firstApplication", "lastApplication"),
  timeRange_Valid2 = c("c(0,30)", NA, "c(0,40)", "lastApplication"),
  timeRange_invalid1 = c("total", "invalid", "firstApplication", "lastApplication"),
  timeRange_invalid1 = c("c(0,30,50)", "total", "firstApplication", "lastApplication"),
  timeRange_invalid1 = c("c(0,NA)", "total", "firstApplication", "lastApplication")
)

# Write unit tests for the function
test_that("validateTimeRangeColumns function test", {
  # Test if the function correctly validates correct TimeRange Columns
  expect_no_error(validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4)]))

  # Test if the function correctly checks for at least one TimeRange Column
  expect_error(validateTimeRangeColumns(configTablePlots[, c(1, 2)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4, 5)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4, 6)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4, 7)]))
})
