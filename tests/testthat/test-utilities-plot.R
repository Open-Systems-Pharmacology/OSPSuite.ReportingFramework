# initialize logging. Is always needed
projectConfiguration <- setUpTestProject()
initLogfunction(projectConfiguration = projectConfiguration, verbose = FALSE)

test_that("generation of default template works", {
  executeAsValidRun(FALSE)
  addDefaultConfigForTimeProfilePlots(
    projectConfiguration = projectConfiguration,
    sheetName = "TimeProfileNew", overwrite = FALSE
  )

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  expect_contains(wb$sheet_names, "TimeProfileNew")
})


# Create example data for testing
mockConfigTable <- function() {
  data.table(
    Level = c(1, 2, NA, NA, NA),
    Header = c("Header1", "Header2", NA, NA, NA),
    Value1 = c(NA, NA, 10, 30, 40),
    Value2 = c(NA, NA, "A", "B", NA)
  )
}

# Write unit tests for the function
test_that("validateHeaders function test", {
  # Test if the function returns the correct configuration table without header lines
  configTable <- mockConfigTable()
  expect_equal(nrow(validateHeaders(configTable)), 3)

  # Test if the function throws an error when the plot configuration table has non-empty columns
  configTable <- mockConfigTable()
  configTable$Value1[1] <- 3
  expect_error(validateHeaders(configTable))

  # Test if the function throws an error when the plot configuration table has missing header for level
  configTable <- mockConfigTable()
  configTable$Header[1] <- NA
  expect_error(validateHeaders(configTable))
})

# Create example data for testing
configTablePlots <- data.table(
  CharacterColumn1 = c("A", "B", "C", "D"),
  CharacterColumn2 = c("X", NA, "Y", "Z"),
  NumericColumn1 = c(1, 2, 3, NA),
  LogicalColumn1 = c(TRUE, FALSE, TRUE, TRUE),
  NumericRangeColumn1 = c("c(1,2)", "c(3,NA)", NA, "c(5,6)")
)

test_that("validateConfigTablePlots function test", {
  expect_no_error(validateConfigTablePlots(configTablePlots,
    charactersWithoutMissing = c("CharacterColumn1"),
    charactersWithMissing = c("CharacterColumn2"),
    numericColumns = "NumericColumn1",
    logicalColumns = "LogicalColumn1",
    numericRangeColumns = "NumericRangeColumn1",
    subsetList = list(list(
      cols = c("CharacterColumn1"),
      allowedValues = c("A", "B", "C", "D")
    ))
  ))


  # Test if the function correctly validates character columns without missing values
  expect_error(validateConfigTablePlots(configTablePlots,
    charactersWithoutMissing = c("CharacterColumn2")
  ))

  # Test if the function correctly validates numeric columns
  expect_error(validateConfigTablePlots(configTablePlots,
    numericColumns = c("CharacterColumn2")
  ))

  # Test if the function correctly validates logical columns
  expect_error(validateConfigTablePlots(configTablePlots,
    logicalColumns = "CharacterColumn1"
  ))

  # Test if the function correctly validates numeric range columns
  expect_error(validateConfigTablePlots(configTablePlots, numericRangeColumns = "NumericColumn1"))

  # Test if the function correctly validates subset list
  expect_error(validateConfigTablePlots(configTablePlots,
    subsetList = list(list(
      cols = c("CharacterColumn2"),
      allowedValues = c("A", "B", "C", "D")
    ))
  ))
})


# Create example data for testing
configTablePlots <- data.table(
  Column1 = c(1, 2, NA, 4),
  Column2 = c("A", "B", NA, "D"),
  Column3 = c(TRUE, FALSE, NA, TRUE)
)

test_that("validateAtleastOneEntry function test", {
  # Test if the function correctly validates that each plot row needs at least one entry in specified columns
  expect_error(validateAtleastOneEntry(configTablePlots, c("Column1", "Column2", "Column3")))
})


# Create example data for testing
dtOutputPaths <- data.table(
  outputPathId = c("id1", "id1", "id2", "id2"),
  DisplayName = c("Display1", "Display1", "Display2", "Display2"),
  DisplayUnit = c("Unit1", "Unit1", "Unit1", "Unit2")
)

# Write unit tests for the function
test_that("validateOutputIdsForPlot function test", {
  # Test if the function correctly find the inconsistency in DisplayUnit
  expect_error(validateOutputIdsForPlot(dtOutputPaths))
})


cleanupLogFileForTest(projectConfiguration)
