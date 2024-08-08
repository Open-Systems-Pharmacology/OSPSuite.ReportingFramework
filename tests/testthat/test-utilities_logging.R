# set up directory with figures
projectPath <- iniLogFileForTest()

# Unit tests for initLogfunction
test_that("initLogfunction creates default log file folder when logFileFolder is NULL", {
  suppressMessages(initLogfunction(projectPath))
  expect_true(file.exists(file.path(projectPath, "Logs")))
})

test_that("initLogfunction creates log file folder with specified path", {
  logFileFolder <- file.path(projectPath, "custom_logs")
  suppressMessages(initLogfunction(projectPath, logFileFolder))
  expect_true(file.exists(logFileFolder))
})

#  tests for `writeToLog`
test_that("writeToLog appends log message to file", {
  suppressMessages(initLogfunction(projectPath))

  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  filename <- "test.log"
  type <- "Info"
  msg <- "Test log message"
  writeToLog(type, msg, filename)
  suppressWarnings(logFile <- readLines(file.path(logFileFolder, filename)))
  expect_true(length(logFile) > 0)
  expect_equal(grep(paste0(type, ": ", msg), tail(logFile, 1)), expected = 1)
})


# Unit tests for logCatch Message
test_that("logCatch function catches only messages to display", {
  myMessage <- "Hide message"
  initLogfunction(
    projectPath = projectPath,
    messagesNotDisplayed = myMessage, verbose = FALSE
  )

  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")
  myFunction <- function(msg) {
    message(msg)
  }

  suppressMessages(logCatch(expr = myFunction(msg = myMessage)))
  logFile <- readLines(file.path(logFileFolder, "run.log"))
  expect_true(length(grep(myMessage, logFile)) == 0) # The message should not be logged

  myMessageShow <- "Show message"
  suppressMessages(logCatch(expr = myFunction(msg = myMessageShow)))
  logFile <- readLines(file.path(logFileFolder, "run.log"))
  expect_true(length(grep(myMessageShow, logFile)) > 0) # The message should not be logged
})


# Unit tests errors and warnings
test_that("logCatch function catches only messages to display", {
  initLogfunction(
    projectPath = projectPath,
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
  suppressMessages(initLogfunction(projectPath = projectPath))

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

  initLogfunction(projectPath = projectPath, verbose = FALSE)
  setShowLogMessages(TRUE)
  output <- utils::capture.output(logCatch(message(myMessage)), type = "message")

  expect_true(output == myMessage)
})


# Define the example data
example_data <- data.table(x = 1:5, y = letters[1:5])

# Write the unit test
test_that("writeTableToLog function works as expected", {

  # Call the function with example data
  writeTableToLog(example_data,filename = 'table.log')

  # Verify that the log file has been created
  # Check if the log file was created and contains the session info
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  suppressWarnings(logContent <- readLines(file.path(logFileFolder, "table.log")))
  expect_true(length(logContent) > 0, "Log file was created")

})

cleanupLogFileForTest(projectPath)
