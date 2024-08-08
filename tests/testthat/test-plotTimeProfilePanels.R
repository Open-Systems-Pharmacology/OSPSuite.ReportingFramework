projectPath <- iniLogFileForTest()
projectConfiguration <- suppressMessages(setUpTestProject(projectPath))

# Create example data for testing
configTablePlots <- data.table(
  PlotName = c("Plot1", "Plot1", "Plot2", "Plot2"),
  Column1 = c("A", "A", "B", "B"),
  Column2 = c("X", "X", "X", "Y"),
  OutputPathIds = c("id1", "id2", "id3", "id4")
)


# Write unit tests for the function
test_that("validatePanelConsistency function test", {
  # Test if the function correctly checks for unique values of panel columns for each PlotName
  expect_error(validatePanelConsistency(configTablePlots, c("Column1", "Column2")))

})


# Create example data for testing
configTablePlots <- data.table(
  Value1 = c(1, 2, 3, 4),
  Value2 = c("A", "B", "C", "D"),
  TimeRange_Valid1 = c(NA, "total", "firstApplication", "lastApplication"),
  TimeRange_Valid2 = c("c(0,30)",NA, "c(0,40)", "lastApplication"),
  TimeRange_invalid1 = c("total", "invalid", "firstApplication", "lastApplication"),
  TimeRange_invalid1 = c("c(0,30,50)", "total", "firstApplication", "lastApplication"),
  TimeRange_invalid1 = c("c(0,NA)", "total", "firstApplication", "lastApplication")
)

# Write unit tests for the function
test_that("validateTimeRangeColumns function test", {
  # Test if the function correctly validates correct TimeRange Columns
  expect_no_error(validateTimeRangeColumns(configTablePlots[,c(1,2,3,4)]))

  # Test if the function correctly checks for at least one TimeRange Column
  expect_error(validateTimeRangeColumns(configTablePlots[,c(1,2)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(validateTimeRangeColumns(configTablePlots[,c(1,2,3,4,5)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(validateTimeRangeColumns(configTablePlots[,c(1,2,3,4,6)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(validateTimeRangeColumns(configTablePlots[,c(1,2,3,4,7)]))

})

