# Unit tests for addMessageToLog
test_that("addMessageToLog logs messages successfully", {
  expect_no_error(addMessageToLog("Test message"))
})

test_that("setLogPath writes to the configured log file", {
  logFolder <- file.path(tempdir(), "rf-custom-log")
  logFile <- "workflow.log"
  logPath <- file.path(logFolder, logFile)

  unlink(logFolder, recursive = TRUE, force = TRUE)
  on.exit(unlink(logFolder, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(ospsuite.utils::setLogFolder(NULL), add = TRUE)

  expect_no_error(setLogPath(logFolder = logFolder, logFileName = logFile))

  addMessageToLog("Custom path message")

  expect_true(file.exists(logPath))
  expect_true(any(grepl(
    "Custom path message",
    readLines(logPath),
    fixed = TRUE
  )))
})

test_that("saveSessionInfo writes a separate file in the configured log folder", {
  logFolder <- file.path(tempdir(), "rf-session-log")
  sessionFile <- "custom-session-info.log"
  sessionPath <- file.path(logFolder, sessionFile)

  unlink(logFolder, recursive = TRUE, force = TRUE)
  on.exit(unlink(logFolder, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(ospsuite.utils::setLogFolder(NULL), add = TRUE)

  expect_no_error(setLogPath(
    logFolder = logFolder,
    logFileName = "workflow.log"
  ))
  expect_no_error(saveSessionInfo(fileName = sessionFile))

  expect_true(file.exists(sessionPath))
  expect_true(any(grepl("R version", readLines(sessionPath), fixed = TRUE)))
})

test_that("saveSessionInfo requires a configured log folder", {
  ospsuite.utils::setLogFolder(NULL)

  expect_error(
    saveSessionInfo(),
    "No log folder configured"
  )
})

# Unit tests errors and warnings
test_that("captureLog function catches messages and errors", {
  # Test message capture
  expect_no_error(captureLog(expr = message("Test info message")))

  # Test warning capture
  expect_no_error(captureLog(expr = warning("Test warning message")))

  # Test error capture
  expect_error(captureLog(expr = stop("Test error message")))
})

# Unit tests for setShowLogMessages
test_that("setShowLogMessages controls verbosity", {
  # Set to non-verbose
  expect_no_error(setShowLogMessages(FALSE))

  # Set to verbose
  expect_no_error(setShowLogMessages(TRUE))
})

# Unit tests for writeTableToLog
test_that("writeTableToLog logs data frames successfully", {
  exampleData <- data.table(x = 1:5, y = letters[1:5])

  # Call the function with example data
  expect_no_error(writeTableToLog(exampleData, filename = "table.log"))
})

test_that("captureLog executes finallyExpression", {
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
