#' Sets options for a reporting frame work workflow.
#'
#' @details
#'
#' This function configures options for a reporting framework workflow, which is typically applied in two distinct phases:
#'
#' 1. Exploratory Analysis: This phase involves preparation of outputs that are not yet qualified. In this case:
#'    - Watermarks are not applied.
#'    - Helper functions for building configuration tables are allowed.
#'
#' 2. Production of Final Output: This phase involves generating the "final" output (a valid run). In this case:
#'    - Figures should not have watermarks.
#'    - Functions that manipulate inputs are not allowed.
#'    - The workflow will stop if an error occurs during execution.
#'
#' Relevant Options:
#'
#' - `ospsuite.plots.watermark_enabled`: Set to TRUE when `isValidRun` is FALSE to display watermarks on figures, and FALSE when `isValidRun` is TRUE.
#'
#' - `OSPSuite.RF.skipFailingPlots`: Set to TRUE when `isValidRun` is FALSE to skip plots that fail to generate, and FALSE when `isValidRun` is TRUE.
#'
#' - `OSPSuite.RF.stopHelperFunction`: Set to TRUE when `isValidRun` is TRUE to stop the execution of helper functions during valid runs.
#'
#' @param isValidRun A logical value indicating if the run is valid. If TRUE,
#'        options are set for a valid run; if FALSE, options are set for an invalid run.
#'        If NULL, the value is derived from the `QCpassed` environment variable.
#' @return Returns invisibly.
#'
#' @export
#' @family project initialization
setWorkflowOptions <- function(isValidRun = NULL) {
  if (is.null(isValidRun)) {
    isValidRun <- getQCpassedEnvironmentVariable()
  }
  checkmate::assertLogical(isValidRun, len = 1)

  # set options to enable watermarks
  options(ospsuite.plots.watermark_enabled = !isValidRun)

  # skip failures in figure generation
  options(OSPSuite.RF.skipFailingPlots = !isValidRun)

  # stop helper functions
  options(OSPSuite.RF.stopHelperFunction = isValidRun)

  return(invisible())
}
#' Get QC Passed Environment Variable
#'
#' This function retrieves the value of the environment variable `QCpassed`.
#' It attempts to convert the value to a logical type. If the environment variable
#' is not set, is empty, or is non-logical, a warning is issued and the function
#' defaults the value to `FALSE`.
#'
#' @return A logical value indicating whether the QC passed (`TRUE` or `FALSE`).
#' @export
#' @family project initialization
#'
#' @examples
#' \dontrun{
#' # Set the environment variable for testing
#' Sys.setenv(QCpassed = "TRUE")
#' getQCpassedEnvironmentVariable() # Should return TRUE
#'
#' # Unset the environment variable for testing
#' Sys.unsetenv("QCpassed")
#' getQCpassedEnvironmentVariable() # Should return FALSE and issue a warning
#' }
getQCpassedEnvironmentVariable <- function() {
  qCpassed <- suppressWarnings(as.logical(as.double(Sys.getenv(
    x = "QCpassed"
  ))))
  # check if QCpassed was set as TRUE or FALSE
  if (is.na(qCpassed)) {
    qCpassed <- as.logical(Sys.getenv(x = "QCpassed"))
  }
  # add warning message for unset or corrupt variable, set to default to avoid further messages
  if (is.na(qCpassed)) {
    warning(messages$warningutilitiesworkflowL1()) # nolint: object_usage_linter
    Sys.setenv(QCpassed = 0)
    qCpassed <- FALSE
  }

  return(qCpassed)
}

#' Stop Helper Function
#'
#' This function checks if the current execution context is valid for running helper functions.
#' If the helper function is being called during a valid run, it raises an error.
#'
#' @details
#' The function retrieves the option `OSPSuite.RF.stopHelperFunction` to determine if helper functions
#' are allowed during a valid run. If the option is not initialized, it prompts the user to call
#' `executeAsValidRun(isValidRun)`.
#'
#' Stops execution with an error message if called during a valid run or if the option is not set.
#'
#' @return Returns invisibly.
#' @keywords internal
#' @noRd
.stopHelperFunction <- function() {
  stopHelperFunction <- getOption("OSPSuite.RF.stopHelperFunction")

  if (is.null(stopHelperFunction)) {
    stop(messages$errorutilitiesworkflowL1()) # nolint: object_usage_linter
  }

  if (stopHelperFunction) {
    stop(messages$errorutilitiesworkflowL1X()) # nolint: object_usage_linter
  }

  return(invisible())
}
