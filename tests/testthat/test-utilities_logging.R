projectPath <- tempdir()
if (!dir.exists(projectPath)) dir.create(projectPath)

# Unit tests for initLogfunction
test_that("initLogfunction creates default logfile folder when logfilefolder is NULL", {
  suppressMessages(initLogfunction(projectPath))
  expect_true(file.exists(file.path(projectPath, "logs")))
})

test_that("initLogfunction creates logfile folder with specified path", {
  logfilefolder <- file.path(projectPath, "custom_logs")
  suppressMessages(initLogfunction(projectPath, logfilefolder))
  expect_true(file.exists(logfilefolder))
})

#  tests for writeToLog
test_that("writeToLog appends log message to file", {
  suppressMessages(initLogfunction(projectPath))

  logfilefolder <- getOption("OSPSuite.REF.logfilefolder")

  filename <- "test.log"
  type <- "Info"
  msg <- "Test log message"
  writeToLog(type, msg, filename)
  suppressWarnings(logFile <- readLines(file.path(logfilefolder, filename)))
  expect_true(length(logFile) > 0)
  expect_equal(grep(paste0(type, ": ", msg), tail(logFile, 1)), expected = 1)
})


# Unit tests for logCatch Message
test_that("logCatch function catches only messages to display", {
  myMessage <- "Hide message"
  initLogfunction(
    projectPath = projectPath,
    messagesNotDisplayed = myMessage, verbose = TRUE
  )

  logfilefolder <- getOption("OSPSuite.REF.logfilefolder")
  myFunction <- function(msg) {
    message(msg)
  }

  suppressMessages(logCatch(expr = myFunction(msg = myMessage)))
  log_file <- readLines(file.path(logfilefolder, "run.log"))
  expect_true(length(grep(myMessage, log_file)) == 0) # The message should not be logged

  myMessageShow <- "Show message"
  suppressMessages(logCatch(expr = myFunction(msg = myMessageShow)))
  log_file <- readLines(file.path(logfilefolder, "run.log"))
  expect_true(length(grep(myMessageShow, log_file)) > 0) # The message should not be logged
})


# Unit tests errors and warnings
test_that("logCatch function catches only messages to display", {
  initLogfunction(
    projectPath = projectPath,
    verbose = FALSE
  )

  logfilefolder <- getOption("OSPSuite.REF.logfilefolder")

  logCatch(expr = warning('Warning message'))
  suppressWarnings(log_file <- readLines(file.path(logfilefolder, "run.log")))
  expect_true(length(grep('Warning message', log_file)) > 0) # The message should not be logged

  expect_error(logCatch(expr = stop('Error message')))
  suppressWarnings(log_file <- readLines(file.path(logfilefolder, "run.log")))
  expect_true(length(grep(myMessageShow, log_file)) > 0) # The message should not be logged
})

test_that("saveSessionInfo writes session info to log file", {
  # Set up log function
  suppressMessages(initLogfunction(projectPath = projectPath))

  # Call the saveSessionInfo function
  saveSessionInfo()

  # Check if the log file was created and contains the session info
  logfilefolder <- getOption("OSPSuite.REF.logfilefolder")

  suppressWarnings(logContent <- readLines(file.path(logfilefolder, "SessionInfo.log")))
  expect_true(length(logContent) > 0, "Log file was created")
  expect_true(any(grepl("Session Info", logContent)), "Session Info was written to log file")
})


# Test for verbose = FALSE
test_that("logCatch logs messages when verbose is TRUE", {
  myMessage <- "Test message"

  initLogfunction(projectPath = projectPath, verbose = FALSE)
  setShowLogMessages(TRUE)
  output <- utils::capture.output(logCatch(message(myMessage)), type = "message")

  expect_true(output == myMessage)
})



# Clean up: delete the temporary directories and files
unlink(projectPath, recursive = TRUE)
options(OSPSuite.REF.logfilefolder = NULL)
options(OSPSuite.REF.warningsNotDisplayed = NULL)
options(OSPSuite.REF.messagesNotDisplayed = NULL)
