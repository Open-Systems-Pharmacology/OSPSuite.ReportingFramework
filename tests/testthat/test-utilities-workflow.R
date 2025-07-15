test_that("stopHelperFunction stops with error when option is NULL", {
  options(OSPSuite.RF.stopHelperFunction = NULL) # Set option to NULL
  expect_error(stopHelperFunction())

  options(OSPSuite.RF.stopHelperFunction = TRUE) # Set option to TRUE
  expect_error(stopHelperFunction())

  options(OSPSuite.RF.stopHelperFunction = FALSE) # Set option to FALSE
  expect_silent(stopHelperFunction()) # Should not throw an error
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
