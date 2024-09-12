#' Sets options to the values they should have for a valid run
#'
#' relevant options are
#'   - watermark for figures (see vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
#'   - OSPSuite.RF.skipFailingPlots
#'   - OSPSuite.RF.stopHelperFunction  stops helper function during valid runs
#'
#'
#' @param isValidRun  if TRUE
#'
#' @export
executeAsValidRun <- function(isValidRun) {
  # set options to enable watermarks
  options(ospsuite.plots.watermark_enabled = !isValidRun)

  # skip failures in figure generation
  options(OSPSuite.RF.skipFailingPlots = !isValidRun)

  # stop helper functions
  options(OSPSuite.RF.stopHelperFunction = isValidRun)

  return(invisible())
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
stopHelperFunction <- function() {
  stopHelperFunction <- getOption("OSPSuite.RF.stopHelperFunction")

  if (is.null(stopHelperFunction)) {
    stop("Option stopHelperFunction to control valid runs is not initialized. Please call `executeAsValidRun(isValidRun)`")
  }

  if (stopHelperFunction) {
    callingFunction <- as.character(sys.call(-1)) # Get the calling function
    stop(paste("You are using a helper function, which is not allowed during a valid run. Called from:", callingFunction))
  }

  return(invisible())
}
