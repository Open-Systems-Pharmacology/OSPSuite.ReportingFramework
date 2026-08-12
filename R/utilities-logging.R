#' @importFrom ospsuite.utils logInfo logWarning logError logDebug logCatch
#' @importFrom ospsuite.utils setLogFolder setWarningMasking setInfoMasking
#' @importFrom logger log_threshold INFO WARN
NULL

#' Initializing a Log Function
#'
#' Initializes logging for a workflow run. Creates a timestamped subfolder under the log
#' directory and configures warning/message masking via `ospsuite.utils`.
#'
#' @param projectConfiguration Object of class `ProjectConfiguration` containing information on paths and file names
#' @param warningsNotDisplayed A list of warnings that should not be logged.
#' @param messagesNotDisplayed A list of messages that should not be logged.
#' @param verbose boolean, if true log messages will be shown on the console
#' @param loggingFolder A character string specifying the folder where log files should be stored. If NULL, defaults to a "Logs" sub-folder within the output folder of the project configuration.
#'
#' @examples
#' \dontrun{
#' initLogfunction(projectConfiguration = myProjectConfig)
#' }
#'
#' @export
#' @family log file management
initLogfunction <- function(
  projectConfiguration,
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
  verbose = TRUE,
  loggingFolder = NULL
) {
  checkmate::assertCharacter(warningsNotDisplayed)
  checkmate::assertCharacter(messagesNotDisplayed)

  if (is.null(loggingFolder)) {
    loggingFolder <- file.path(projectConfiguration$outputFolder, "Logs")
  }
  if (!dir.exists(loggingFolder)) {
    dir.create(loggingFolder, recursive = TRUE)
  }

  scriptName <- tryCatch(
    {
      script <- sys.frame(1)$ofile
      sub(".R$", "", basename(script))
    },
    error = function(e) NULL
  )

  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  logFileFolder <- file.path(
    loggingFolder,
    paste(scriptName, timestamp, sep = "_")
  )
  if (!dir.exists(logFileFolder)) {
    dir.create(logFileFolder, recursive = TRUE)
  }

  ospsuite.utils::setWarningMasking(warningsNotDisplayed)
  ospsuite.utils::setInfoMasking(messagesNotDisplayed)
  ospsuite.utils::setLogFolder(logFileFolder)

  if (!verbose) {
    logger::log_threshold(logger::WARN, index = 1)
  }

  ospsuite.utils::logInfo("Start run of workflow")

  optionstxt <- paste0(
    "\nOptions for workflow:\n",
    "OSPSuite.plots.watermark_enabled: ",
    ospsuite.plots::getOspsuite.plots.option(
      ospsuite.plots::OptionKeys$watermark_enabled
    ),
    "\n",
    "OSPSuite.RF.skipFailingPlots: ",
    ifelse(
      getOption("OSPSuite.RF.skipFailingPlots", default = FALSE),
      "Failing Plots are skipped with warning",
      "Failing Plots throw errors"
    ),
    "\n",
    "OSPSuite.RF.stopHelperFunction: ",
    ifelse(
      getOption("OSPSuite.RF.stopHelperFunction", default = FALSE),
      "Stops in helper functions",
      "Workflow executes helper functions"
    )
  )
  ospsuite.utils::logInfo(optionstxt)
}

#' Add a message to the log file
#'
#' @param messageText character with message text
#'
#' @export
#' @family log file management
addMessageToLog <- function(messageText) {
  ospsuite.utils::logInfo(messageText)
}

#' Catch messages, warnings, and errors and route them to the log
#'
#' Wraps `ospsuite.utils::logCatch` and preserves an optional `finally` expression
#' for backwards compatibility.
#'
#' @param expr The expression to evaluate.
#' @param finallyExpression The expression to evaluate finally.
#'
#' @export
#' @family log file management
captureLog <- function(expr, finallyExpression = invisible()) {
  tryCatch(
    ospsuite.utils::logCatch(expr),
    error = function(e) stop(e$message, call. = FALSE),
    finally = finallyExpression
  )
  return(invisible())
}

#' Write a data frame to the log
#'
#' @param dt table to log
#' @param filename Ignored; kept for backwards compatibility.
#'
#' @export
#' @family log file management
writeTableToLog <- function(dt, filename = NULL) {
  checkmate::assertDataFrame(dt)
  ospsuite.utils::logInfo(paste(
    utils::capture.output(print(dt)),
    collapse = "\n"
  ))
  return(invisible())
}

#' Toggle display of log messages on the console
#'
#' @param verbose boolean, if true log messages will be shown
#'
#' @export
#' @family log file management
setShowLogMessages <- function(verbose = TRUE) {
  if (verbose) {
    logger::log_threshold(logger::INFO, index = 1)
  } else {
    logger::log_threshold(logger::WARN, index = 1)
  }
}

#' Save session information to the log
#'
#' @examples
#' \dontrun{
#' saveSessionInfo()
#' }
#'
#' @export
#' @family log file management
saveSessionInfo <- function() {
  sessionInfoText <- paste(
    utils::capture.output(sessionInfo()),
    collapse = "\n"
  )
  ospsuite.utils::logInfo(paste("Session Info:\n", sessionInfoText))
}
