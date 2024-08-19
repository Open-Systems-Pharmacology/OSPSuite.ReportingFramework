# set up directory with figures
projectConfiguration <- setUpTestProject()

# Unit tests for initLogfunction
test_that("initLogfunction creates default log file folder when logFileFolder is NULL", {
  suppressMessages(initLogfunction(projectConfiguration))
  expect_true(dir.exists(file.path(projectConfiguration$outputFolder,"Logs")))
})

#  tests for `writeToLog`
test_that("writeToLog appends log message to file", {
  suppressMessages(initLogfunction(projectConfiguration))

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


# Define the example data
example_data <- data.table(x = 1:5, y = letters[1:5])

# Write the unit test
test_that("writeTableToLog function works as expected", {

  setShowLogMessages(FALSE)
  # Call the function with example data
  writeTableToLog(example_data,filename = 'table.log')

  # Verify that the log file has been created
  # Check if the log file was created and contains the session info
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")

  suppressWarnings(logContent <- readLines(file.path(logFileFolder, "table.log")))
  expect_true(length(logContent) > 0, "Log file was created")

})

cleanupLogFileForTest(projectConfiguration)
