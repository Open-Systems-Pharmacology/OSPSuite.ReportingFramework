# Tests for messages.R

test_that("messages list exists and is a list", {
  expect_true(is.list(messages))
})

# Test a few representative message functions from different categories

# Aggregation messages
test_that("errorLegendSizeNotCovered returns correct message", {
  result <- messages$errorLegendSizeNotCovered()
  expect_type(result, "character")
  expect_true(grepl("legendsize", result))
})

# Plot configuration messages
test_that("errorNoValidScaleVector returns correct message with parameter", {
  result <- messages$errorNoValidScaleVector(c("scale1", "scale2"))
  expect_type(result, "character")
  expect_true(grepl("scale1", result))
  expect_true(grepl("scale2", result))
})

test_that("errorTooManyColors returns correct message with parameter", {
  result <- messages$errorTooManyColors(10)
  expect_type(result, "character")
  expect_true(grepl("10", result))
})

# PK Parameter messages
test_that("errorEmptyDisplayUnit returns correct message", {
  result <- messages$errorEmptyDisplayUnit()
  expect_type(result, "character")
  expect_true(grepl("displayUnit", result))
})

test_that("errorPKParameterNotDefined returns correct message", {
  result <- messages$errorPKParameterNotDefined("AUC")
  expect_type(result, "character")
  expect_true(grepl("AUC", result))
})

# Data validation messages
test_that("warningDataQuality returns correct message", {
  result <- messages$warningDataQuality("data.csv", "ID123")
  expect_type(result, "character")
  expect_true(grepl("data.csv", result))
  expect_true(grepl("ID123", result))
})

test_that("warningDataContainsNA returns correct message", {
  result <- messages$warningDataContainsNA("concentration")
  expect_type(result, "character")
  expect_true(grepl("concentration", result))
})

# Workflow messages
test_that("errorStopHelperFunctionNotInitialized returns correct message", {
  result <- messages$errorStopHelperFunctionNotInitialized()
  expect_type(result, "character")
  expect_true(grepl("stopHelperFunction", result))
})

# Time profile messages
test_that("errorNoDataForPlot returns correct message", {
  result <- messages$errorNoDataForPlot()
  expect_type(result, "character")
  expect_true(grepl("No data", result))
})

test_that("errorNoSimulatedDataFound returns correct message", {
  result <- messages$errorNoSimulatedDataFound("Plot1")
  expect_type(result, "character")
  expect_true(grepl("Plot1", result))
})

# Markdown messages
test_that("errorNoFileExistsForKey returns correct message", {
  result <- messages$errorNoFileExistsForKey("fig.png", "table.csv")
  expect_type(result, "character")
  expect_true(grepl("fig.png", result))
  expect_true(grepl("table.csv", result))
})

# Reporting messages
test_that("errorPandocNotInstalled returns correct message", {
  result <- messages$errorPandocNotInstalled()
  expect_type(result, "character")
  expect_true(grepl("Pandoc", result))
})

# WorkflowScriptExporter messages
test_that("errorProvideOnlyOneScenarioOrWorkflow returns correct message", {
  result <- messages$errorProvideOnlyOneScenarioOrWorkflow()
  expect_type(result, "character")
  expect_true(grepl("scenario", result) || grepl("workflow", result))
})

# XLSX messages
test_that("warningSheetAlreadyExists returns correct message", {
  result <- messages$warningSheetAlreadyExists("Sheet1")
  expect_type(result, "character")
  expect_true(grepl("Sheet1", result))
})

test_that("errorSheetDoesNotExist returns correct message", {
  result <- messages$errorSheetDoesNotExist("MissingSheet")
  expect_type(result, "character")
  expect_true(grepl("MissingSheet", result))
})

# RmdPlotManager messages
test_that("errorProvideValidRmdName returns correct message", {
  result <- messages$errorProvideValidRmdName()
  expect_type(result, "character")
  expect_true(grepl("Rmd", result))
})

test_that("errorKeyAlreadyAdded returns correct message", {
  result <- messages$errorKeyAlreadyAdded("duplicateKey")
  expect_type(result, "character")
  expect_true(grepl("duplicateKey", result))
})

# Logging messages
test_that("warningLogfileNotInitialized returns correct message", {
  result <- messages$warningLogfileNotInitialized()
  expect_type(result, "character")
  expect_true(grepl("Logfile", result))
})

# Population messages
test_that("messageShiftVirtualTwinPopulation returns correct message", {
  result <- messages$messageShiftVirtualTwinPopulation()
  expect_type(result, "character")
  expect_true(grepl("VirtualTwinPopulation", result))
})

test_that("errorPopulationNotUnique returns correct message", {
  result <- messages$errorPopulationNotUnique("pop.xlsx", c("Pop1", "Pop2"))
  expect_type(result, "character")
  expect_true(grepl("Pop1", result))
  expect_true(grepl("Pop2", result))
})

# Test that all message functions are callable
test_that("all message functions can be called", {
  # Get all names from messages list
  messageNames <- names(messages)
  
  # Check that there are message functions defined
  expect_true(length(messageNames) > 0)
  
  # Check that all are functions
  for (name in messageNames) {
    expect_true(is.function(messages[[name]]), 
                info = paste("messages$", name, " should be a function", sep = ""))
  }
})
