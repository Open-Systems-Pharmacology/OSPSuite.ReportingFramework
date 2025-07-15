# testProject was set up by setup.R

# Unit tests for initLogfunction
test_that("initLogfunction creates default log file folder when logFileFolder is NULL", {
  expect_true(dir.exists(file.path(projectConfiguration$outputFolder, "Logs")))
})

#  tests for `writeToLog`
test_that("writeToLog appends log message to file", {
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  filename <- "test.log"
  type <- "Info"
  msg <- "Test log message"
  writeToLog(type, msg, filename)
  suppressWarnings(logFile <- readLines(file.path(logFileFolder, filename)))
  expect_true(length(logFile) > 0)
  expect_equal(grep(paste0(type, ": ", msg), utils::tail(logFile, 1)), expected = 1)
})


# Unit tests errors and warnings
test_that("logCatch function catches only messages to display", {
  initLogfunction(
    projectConfiguration = projectConfiguration,
    verbose = FALSE
  )

  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  logCatch(expr = warning("Warning message"))
  suppressWarnings(logFile <- readLines(file.path(logFileFolder, "run.log")))
  expect_true(length(grep("Warning message", logFile)) > 0) # The message should be logged

  expect_error(logCatch(expr = stop("Error message")))
  suppressWarnings(logFile <- readLines(file.path(logFileFolder, "run.log")))
  expect_true(length(grep("Error message", logFile)) > 0) # The message should be logged
})

test_that("saveSessionInfo writes session info to log file", {
  # Set up log function
  suppressMessages(initLogfunction(projectConfiguration = projectConfiguration))

  # Call the saveSessionInfo function
  saveSessionInfo()

  # Check if the log file was created and contains the session info
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  suppressWarnings(logContent <- readLines(file.path(logFileFolder, "SessionInfo.log")))
  expect_true(length(logContent) > 0, "Log file was created")
  expect_true(any(grepl("Session Info", logContent)), "Session Info was written to log file")
})


# Test for verbose = FALSE
test_that("logCatch Logs messages when verbose is TRUE", {
  myMessage <- "Test message"

  initLogfunction(projectConfiguration = projectConfiguration, verbose = FALSE)
  setShowLogMessages(TRUE)
  output <- utils::capture.output(logCatch(message(myMessage)), type = "message")

  expect_true(output == myMessage)
})


exampleData <- data.table(x = 1:5, y = letters[1:5])

test_that("writeTableToLog function works as expected", {
  setShowLogMessages(FALSE)
  # Call the function with example data
  writeTableToLog(exampleData, filename = "table.log")

  # Verify that the log file has been created
  # Check if the log file was created and contains the session info
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  suppressWarnings(logContent <- readLines(file.path(logFileFolder, "table.log")))
  expect_true(length(logContent) > 0, "Log file was created")
})

test_that("logCatch executes finallyExpression", {
  # Initialize a variable to track if the finallyExpression was executed
  finallyExecuted <- FALSE

  # Call logCatch with an expression that generates an error
  expect_error(logCatch(expr = stop("This is an error"), finallyExpression = finallyExecuted <<- TRUE))

  # Check if the finally expression was executed
  expect_true(finallyExecuted, "The finallyExpression should be executed even after an error")

  # Reset for a successful case
  finallyExecuted <- FALSE

  # Call logCatch with a successful expression
  logCatch(expr = {
    a <- 1
  }, finallyExpression = {
    finallyExecuted <<- TRUE
  })

  # Check if the finally expression was executed
  expect_true(finallyExecuted, "The finallyExpression should be executed after a successful expression")
})
