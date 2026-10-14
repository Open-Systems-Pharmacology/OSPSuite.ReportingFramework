#' @importFrom ospsuite.utils logInfo logWarning logError logDebug logCatch
#' @importFrom logger log_threshold INFO WARN
NULL

#' Add a message to the log file
#'
#' @param messageText character with message text
#'
#' @export
#' @family log file management
addMessageToLog <- function(messageText) {
  ospsuite.utils::logInfo(messageText)
}

#' Set log folder and file name
#'
#' Configures file logging through the `ospsuite.utils` logging backend and
#' overrides the default `log.txt` target with a caller-defined file name.
#'
#' @param logFolder character string with the folder for log files. If `NULL`,
#'   file logging is disabled.
#' @param logFileName character string with the log file name.
#'
#' @export
#' @family log file management
setLogPath <- function(logFolder = NULL, logFileName = "log.txt") {
  checkmate::assertCharacter(logFileName, any.missing = FALSE, len = 1)

  if (is.null(logFolder)) {
    ospsuite.utils::setLogFolder(NULL)
    return(invisible())
  }

  checkmate::assertCharacter(logFolder, any.missing = FALSE, len = 1)

  if (!dir.exists(logFolder)) {
    dir.create(logFolder, recursive = TRUE)
  }

  ospsuite.utils::setLogFolder(logFolder)
  logger::log_appender(
    logger::appender_file(file.path(logFolder, logFileName)),
    index = 2
  )

  return(invisible())
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
#' @param fileName character string with the session info file name.
#'
#' @examples
#' \dontrun{
#' saveSessionInfo()
#' }
#'
#' @export
#' @family log file management
saveSessionInfo <- function(fileName = "sessionInfo.log") {
  checkmate::assertCharacter(fileName, any.missing = FALSE, len = 1)

  sessionInfoText <- paste(
    utils::capture.output(utils::sessionInfo()),
    collapse = "\n"
  )

  logFolder <- ospsuite.utils::getLogFolder()

  if (is.null(logFolder) || !nzchar(logFolder)) {
    stop(
      "No log folder configured. Call setLogPath() before saveSessionInfo()."
    )
  }

  writeLines(
    text = sessionInfoText,
    con = file.path(logFolder, fileName)
  )

  return(invisible())
}
