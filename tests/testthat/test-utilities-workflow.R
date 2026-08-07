test_that("setWorkflowOptions sets options for valid and exploratory runs", {
  options(
    ospsuite.plots.watermark_enabled = NULL,
    OSPSuite.RF.skipFailingPlots = NULL,
    OSPSuite.RF.stopHelperFunction = NULL
  )

  expect_silent(setWorkflowOptions(isValidRun = TRUE))
  expect_false(getOption("ospsuite.plots.watermark_enabled"))
  expect_false(getOption("OSPSuite.RF.skipFailingPlots"))
  expect_true(getOption("OSPSuite.RF.stopHelperFunction"))

  expect_silent(setWorkflowOptions(isValidRun = FALSE))
  expect_true(getOption("ospsuite.plots.watermark_enabled"))
  expect_true(getOption("OSPSuite.RF.skipFailingPlots"))
  expect_false(getOption("OSPSuite.RF.stopHelperFunction"))
})

test_that("setWorkflowOptions rejects non-logical input", {
  expect_error(setWorkflowOptions(isValidRun = "TRUE"))
  expect_error(setWorkflowOptions(isValidRun = c(TRUE, FALSE)))
})

test_that("setWorkflowOptions derives isValidRun from QCpassed when NULL", {
  Sys.setenv(QCpassed = "TRUE")
  expect_silent(setWorkflowOptions(isValidRun = NULL))
  expect_false(getOption("ospsuite.plots.watermark_enabled"))
  expect_false(getOption("OSPSuite.RF.skipFailingPlots"))
  expect_true(getOption("OSPSuite.RF.stopHelperFunction"))

  Sys.setenv(QCpassed = "FALSE")
  expect_silent(setWorkflowOptions(isValidRun = NULL))
  expect_true(getOption("ospsuite.plots.watermark_enabled"))
  expect_true(getOption("OSPSuite.RF.skipFailingPlots"))
  expect_false(getOption("OSPSuite.RF.stopHelperFunction"))

  Sys.unsetenv("QCpassed")
})

test_that(".stopHelperFunction stops with error when option is NULL", {
  options(OSPSuite.RF.stopHelperFunction = NULL) # Set option to NULL
  expect_error(.stopHelperFunction())

  options(OSPSuite.RF.stopHelperFunction = TRUE) # Set option to TRUE
  expect_error(.stopHelperFunction())

  options(OSPSuite.RF.stopHelperFunction = FALSE) # Set option to FALSE
  expect_silent(.stopHelperFunction()) # Should not throw an error
})
test_that("getQCpassedEnvironmentVariable works correctly", {
  # Test when QCpassed is set to "TRUE"
  Sys.setenv(QCpassed = "TRUE")
  expect_equal(getQCpassedEnvironmentVariable(), TRUE)

  # Test when QCpassed is set to "FALSE"
  Sys.setenv(QCpassed = "FALSE")
  expect_equal(getQCpassedEnvironmentVariable(), FALSE)

  # Test when QCpassed is set to an invalid value
  Sys.setenv(QCpassed = "invalid")
  expect_warning(
    {
      result <- getQCpassedEnvironmentVariable()
    },
    "Environment Variable 'QCpassed' not found, empty or a non logical, set 'QCpassed' to FALSE"
  )
  expect_equal(result, FALSE)

  # Test when QCpassed is not set
  Sys.unsetenv("QCpassed")
  expect_warning(
    {
      result <- getQCpassedEnvironmentVariable()
    },
    "Environment Variable 'QCpassed' not found, empty or a non logical, set 'QCpassed' to FALSE"
  )
  expect_equal(result, FALSE)

  # Clean up environment variable
  Sys.unsetenv("QCpassed")
})
