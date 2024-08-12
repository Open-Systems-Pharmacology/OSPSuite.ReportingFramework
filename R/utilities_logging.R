#' Initializing a Log Function
#'
#' This function initialize the logging during a workflow. It is called at the start of the workflow script.
#' It is used to configure options for the log file folder, warning swhich should not logged and messages which should not logged.
#'
#' @param loggingFolder The path where the default log file folder is generated.
#' @param logFileSubFolder Optional. If NULL, a default log file folder is generated in the `loggingFolder`timestamp`.
#' @param warningsNotDisplayed A list of warnings that should not be logged.
#' @param messagesNotDisplayed A list of messages that should not be logged.
#' @param verbose boolean, if true log message will be shown on the console
#'
#' @examples
#' \dontrun{
#' # Initialize the log function
#' logFunction <- initLogfunction(projectPath = "path/to/project")
#' }
#'
#' @export
initLogfunction <- function(loggingFolder = file.path('Logs'),
                            logFileSubFolder = NULL,
                            warningsNotDisplayed = c(
                              "introduced infinite values",
                              "Each group consists of only one observation",
                              "rows containing non-finite values",
                              "rows containing missing values",
                              "Ignoring unknown parameters",
                              "was deprecated in ggplot2",
                              "font family not found in Windows font database",
                              # warning thrown because of non-ASCII unicode characters
                              "mbcsToSbcs"
                            ),
                            messagesNotDisplayed = c(
                              "Each group consists of only one observation"
                            ),
                            verbose = TRUE) {
  checkmate::assertCharacter(logFileSubFolder, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(warningsNotDisplayed)
  checkmate::assertCharacter(messagesNotDisplayed)

  if (is.null(logFileSubFolder)) {
    if (!dir.exists(loggingFolder)) dir.create(loggingFolder,recursive = TRUE)
    # Create the log file sub-folder with a time stamp

    # Get the name of the original script
    script_name <- tryCatch({
      script <- sys.frame(1)$ofile
      sub(".R$", "", basename(script))
    }, error = function(e) {
      return(NULL)
    })

    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    logFileSubFolder <- paste(script_name,timestamp,sep = '_')
  }

  logFileFolder <- fs::path_abs(file.path(loggingFolder,logFileSubFolder))

  # Create the log file sub-folder if it doesn't exist
  if (!dir.exists(logFileFolder)) {
    dir.create(logFileFolder, recursive = TRUE)
  }

  # set inputs to options for use in `logCatch` function and `writeToLog`
  options(list(
    OSPSuite.RF.logFileFolder = logFileFolder,
    OSPSuite.RF.warningsNotDisplayed = warningsNotDisplayed,
    OSPSuite.RF.messagesNotDisplayed = messagesNotDisplayed,
    OSPSuite.RF.verbose = verbose
  ))


  # startlogfile
  addMessageToLog("Start run of workflow")
}

#' Used to add message to log file
#'
#' This function is for the usage outside a `logCatch` bracket.
#' Inside the bracket `message("my message Text")` can be used
#'
#' @param messageText character with message text
#'
#' @export
addMessageToLog <- function(messageText) {
  logCatch(
    expr = message(messageText)
  )
}




#' function that catches messages, warnings, and errors.
#' This function has to be initialized by  `initLogfunction` function
#'
#' @param expr The expression to evaluate.
#'
#' @export
logCatch <- function(expr) {
  warningsNotDisplayed <- getOption("OSPSuite.RF.warningsNotDisplayed", default = c())
  messagesNotDisplayed <- getOption("OSPSuite.RF.messagesNotDisplayed", default = c())
  verbose <- getOption("OSPSuite.RF.verbose", default = TRUE)

  tryCatch(
    {
      withCallingHandlers(
        expr,
        error = function(e) {
          calls <- sys.calls()
          errorTrace <- "Error Trace:"
          for (call in calls) {
            textCall <- deparse(call, nlines = 1)
            callNotDisplayed <- any(sapply(
              c("logCatch", "qualificationCatch", "stop", "tryCatch", "withCallingHandlers", "simpleError", "eval\\(ei, envir\\)"),
              FUN = function(pattern) {
                grepl(textCall, pattern = pattern, ignore.case = TRUE)
              }
            ))
            if (callNotDisplayed) {
              next
            }
            errorTrace <- c(errorTrace, textCall)
          }
          errorMessage <- paste0(paste(c(
            e$message,
            errorTrace
          ), collapse = "\n"), "\n")
          writeToLog(
            type = "Error",
            msg = errorMessage
          )
          stop(e)
        },
        warning = function(w) {
          if (!(gsub("\n", "", w$message) %in% warningsNotDisplayed)) {
            warningMessage <- paste0(paste(c(
              w$message
            ), collapse = "\n"), "\n")

            writeToLog(
              type = "Warning",
              msg = warningMessage
            )
          }
          if (!verbose) {
            tryInvokeRestart("muffleWarning")
          }
        },
        message = function(m) {
          if (!(gsub("\n", "", m$message) %in% messagesNotDisplayed)) {
            writeToLog(
              type = "Info",
              msg = m$message
            )
          }
          if (!verbose) {
            tryInvokeRestart("muffleMessage")
          }
        }
      )
    },
    error = function(e) {
      stop(e$message, call. = FALSE)
      stop(e)
    }
  )

  return(invisible())
}

#'
#' Writing to Log
#'
#' The `writeToLog` function is used to append log messages to a log file.
#' The path for the logfile has to be initialized by the function initLogfile
#'
#' @param type The type of message (e.g., Error, Info).
#' @param msg The message to be logged.
#' @param filename The name of the log file.
#'
#' @examples
#' \dontrun{
#' # Write a log message
#' writeToLog(type = "Info", msg = "This is an information message", filename = "run.log")
#' }
#'
writeToLog <- function(type, msg, filename = NULL) {
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")
  if (is.null(filename)) filename <- "run.log"
  checkmate::assertCharacter(type, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(msg)
  checkmate::assertDirectoryExists(logFileFolder)
  checkmate::assertCharacter(filename, len = 1, any.missing = FALSE)

  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    paste0(type, ":"),
    paste(msg, collapse = "\n"),
    file = file.path(logFileFolder, filename),
    append = TRUE
  )
}

#' write a table to the logfile
#'
#' @param dt table to log
#' @param filename filename of log
#'
#' @export
writeTableToLog <- function(dt, filename = "run.log") {
  logFileFolder <- getOption("OSPSuite.RF.logFileFolder")
  verbose <- getOption("OSPSuite.RF.verbose", default = TRUE)

  checkmate::assertDataFrame(dt)
  checkmate::assertDirectoryExists(logFileFolder)
  checkmate::assertCharacter(filename, len = 1, any.missing = FALSE)

  sink(file.path(logFileFolder, filename),append = TRUE,split = FALSE)
  print(dt)
  sink()

  if (verbose) {
    print(dt)
  }

}


#' Function to switch the display of log messages on the console on and off
#'
#' @param verbose boolean, if true log message will be shown
#'
#' @export
setShowLogMessages <- function(verbose = TRUE) {
  options(OSPSuite.RF.verbose = verbose)
}



#'
#' Save Session Info
#'
#' This function can be called at the end of your script to save the session information,
#' including the loaded packages and R version, into a log file.
#' The path for the log file has to be initialized by  the `initLogfunction`
#'
#'
#' @examples
#' \dontrun{
#' # Save session info to log file
#' saveSessionInfo()
#' }
#'
#' @export
saveSessionInfo <- function() {
  sessionInfo <- paste(utils::capture.output(sessionInfo()), collapse = "\n")

  # Write session info to log file
  writeToLog(
    type = "Session Info",
    msg = sessionInfo,
    filename = "SessionInfo.log"
  )
}
