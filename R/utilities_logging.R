#' Initializing a Log Function
#'
#' This function initialize the logging during a workflow. It is called at the start of the workflow script.
#' It is used to configure options for the logfilefolder, warningsNotDisplayed which should not logged and messages which should not logged.
#'
#' @param projectPath The path where the default logfile folder is generated.
#' @param logfilefolder Optional. If NULL, a default logfile folder is generated in the projectPath/logs/timestamp.
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
initLogfunction <- function(projectPath,
                            logfilefolder = NULL,
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
  if (is.null(logfilefolder)) checkmate::assertDirectoryExists(projectPath)
  checkmate::assertCharacter(logfilefolder, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(warningsNotDisplayed)
  checkmate::assertCharacter(messagesNotDisplayed)

  if (is.null(logfilefolder)) {
    # Create the logfile subfolder with a timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    logfilefolder <- file.path(projectPath, "logs", timestamp)
  }

  # Create the logfile subfolder if it doesn't exist
  if (!file.exists(logfilefolder)) {
    dir.create(logfilefolder, recursive = TRUE)
  }

  # set inputs to options for use in logCatch function and writeToLog
  options(list(
    OSPSuite.REF.logfilefolder = logfilefolder,
    OSPSuite.REF.warningsNotDisplayed = warningsNotDisplayed,
    OSPSuite.REF.messagesNotDisplayed = messagesNotDisplayed,
    OSPSuite.REF.verbose = verbose
  ))


  # startlogfile
  addMessageToLog("Start run of workflow")
}

#' Used to add message to log file
#'
#' This function is for the usage outside a logCatch bracket.
#' Inside message(messageText) can be used
#'
#' @param messageText character whit message text
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
  warningsNotDisplayed <- getOption("OSPSuite.REF.warningsNotDisplayed", default = c())
  messagesNotDisplayed <- getOption("OSPSuite.REF.messagesNotDisplayed", default = c())
  verbose <- getOption("OSPSuite.REF.verbose", default = TRUE)

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
            errorTrace <- c(errorTrace,textCall)
          }
          errorMessage <- paste0(paste(c(
            e$message,
            errorTrace
          ), collapse = '\n'),'\n')
          writeToLog(
            type = 'Error',
            msg = errorMessage
          )
          stop(e)
        },
        warning = function(w) {
          if (!(gsub("\n", "", w$message) %in% warningsNotDisplayed)) {
            warningMessage <- paste0(paste(c(
              w$message), collapse = '\n'),'\n')

            writeToLog(
              type = 'Warning',
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
              type = 'Info',
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
  logfilefolder <- getOption("OSPSuite.REF.logfilefolder")
  if (is.null(filename)) filename <- "run.log"

  checkmate::assertCharacter(type, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(msg)
  checkmate::assertDirectoryExists(logfilefolder)
  checkmate::assertCharacter(filename, len = 1, any.missing = FALSE)

  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    paste0(type, ":"),
    paste(msg,collapse = '\n'),
    file = file.path(logfilefolder, filename),
    append = TRUE
  )
}

#' Function to switch the display of log messages on the console on and off
#'
#' @param verbose boolean, if true log message will be shown
#'
#' @export
setShowLogMessages <- function(verbose = TRUE) {
  options(OSPSuite.REF.verbose = verbose)
}



#'
#' Save Session Info
#'
#' This function can be called at the end of your script to save the session information,
#' including the loaded packages and R version, into a log file.
#' The path for the logfile has to be initilaized by  the initLogfunction
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
