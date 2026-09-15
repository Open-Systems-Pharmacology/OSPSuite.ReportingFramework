# Unit tests for initLogfunction
test_that("initLogfunction initializes logging successfully", {
  projectConfiguration <- list(outputFolder = tempdir())

  # Should not raise an error
  expect_no_error(
    initLogfunction(
      projectConfiguration = projectConfiguration,
      verbose = TRUE
    )
  )
})

# Unit tests for addMessageToLog
test_that("addMessageToLog logs messages successfully", {
  projectConfiguration <- list(outputFolder = tempdir())

  # Initialize logging
  initLogfunction(
    projectConfiguration = projectConfiguration,
    verbose = FALSE,
    loggingFolder = tempdir()
  )

  # Capture output from logging message
  expect_no_error(addMessageToLog("Test message"))
})

# Unit tests errors and warnings
test_that("captureLog function catches messages and errors", {
  projectConfiguration <- list(outputFolder = tempdir())

  # Initialize logging
  initLogfunction(
    projectConfiguration = projectConfiguration,
    verbose = FALSE,
    loggingFolder = tempdir()
  )

  # Test message capture
  expect_no_error(captureLog(expr = message("Test info message")))

  # Test warning capture
  expect_no_error(captureLog(expr = warning("Test warning message")))

  # Test error capture
  expect_error(captureLog(expr = stop("Test error message")))
})

# Unit tests for setShowLogMessages
test_that("setShowLogMessages controls verbosity", {
  projectConfiguration <- list(outputFolder = tempdir())

  # Initialize logging
  initLogfunction(
    projectConfiguration = projectConfiguration,
    verbose = TRUE,
    loggingFolder = tempdir()
  )

  # Set to non-verbose
  expect_no_error(setShowLogMessages(FALSE))

  # Set to verbose
  expect_no_error(setShowLogMessages(TRUE))
})

# Unit tests for writeTableToLog
test_that("writeTableToLog logs data frames successfully", {
  projectConfiguration <- list(outputFolder = tempdir())

  # Initialize logging
  initLogfunction(
    projectConfiguration = projectConfiguration,
    verbose = FALSE,
    loggingFolder = tempdir()
  )

  exampleData <- data.table(x = 1:5, y = letters[1:5])

  # Call the function with example data
  expect_no_error(writeTableToLog(exampleData, filename = "table.log"))
})

test_that("captureLog executes finallyExpression", {
  projectConfiguration <- list(outputFolder = tempdir())

  # Initialize logging
  initLogfunction(
    projectConfiguration = projectConfiguration,
    verbose = FALSE,
    loggingFolder = tempdir()
  )

  # Call captureLog with an expression that generates an error
  expect_warning(
    expect_error(captureLog(
      expr = stop("This is an error"),
      finallyExpression = warning("finallyExecuted")
    )),
    "finallyExecuted"
  )

  # Call captureLog with a successful expression
  expect_no_error(captureLog(
    expr = {
      a <- 1
    },
    finallyExpression = {
      message("finallyExecuted")
    }
  ))
})
