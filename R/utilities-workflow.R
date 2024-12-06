#' Sets options for a reporting frame work workflow.
#'
#' Typically a workflow is applied in two different cases.
#' 1. As valid run, the result of a workflow is intended as a package,
#'    which can be used to generate final reports, in this case figures
#'    should not have watermarks, functions which manipulate inputs are not allowed,
#'    and the workflow stops if an error occurs during.
#' 2. For exploratory analysis or preparation of a valid run.
#'
#' A workflow can also produce a ePackage (see vignette XXX)
#' This function configures several options that affect the behavior of the
#' reporting frame work workflow. It allows for the enabling or disabling of watermarks,
#' controls the handling of failing plots, manages the execution of helper functions,
#' and specifies ePackage generation and plot export options.
#'
#' Relevant options include:
#'
#' - `ospsuite.plots.watermark_enabled`: Set to TRUE when `isValidRun` is FALSE
#'   to display watermarks on figures, and FALSE when `isValidRun` is TRUE.
#'
#' - `OSPSuite.RF.skipFailingPlots`: Set to TRUE when `isValidRun` is FALSE
#'   to skip plots that fail to generate, and FALSE when `isValidRun` is TRUE.
#'
#' - `OSPSuite.RF.stopHelperFunction`: Set to TRUE when `isValidRun` is TRUE
#'   to stop the execution of helper functions during valid runs.
#'
#' - `OSPSuite.RF.withEPackage`: Set to TRUE if `ePackageGeneration` is either
#'   'withEpackage' or 'onlyEpackage', indicating that EPackage generation should occur.
#'
#' - `OSPSuite.RF.withPlotExport`: Set to TRUE if `ePackageGeneration` is either
#'   'None' or 'withEpackage', indicating that plot export should occur.
#'
#' @param isValidRun A logical value indicating if the run is valid. If TRUE,
#'        options are set for a valid run; if FALSE, options are set for an invalid run.
#' @param ePackageGeneration A character string indicating the type of ePackage generation.
#'        Options include:
#'
#'        - 'None': No ePackage generation.
#'
#'        - 'withEPackage': Generate ePackage alongside the run.
#'
#'        - 'onlyEPackage': Only generate ePackage without exporting figures.
#'
#' @export
setWorkflowOptions <- function(isValidRun,ePackageGeneration = c('None','withEPackage','onlyEPackage')) {

  match.arg(ePackageGeneration)

  # set options to enable watermarks
  options(ospsuite.plots.watermark_enabled = !isValidRun)

  # skip failures in figure generation
  options(OSPSuite.RF.skipFailingPlots = !isValidRun)

  # stop helper functions
  options(OSPSuite.RF.stopHelperFunction = isValidRun)

  # generateEpackages
  options(OSPSuite.RF.withEPackage = ePackageGeneration %in% c('withEPackage','onlyEPackage'))

  # plotAndTableExport
  options(OSPSuite.RF.withPlotExport = ePackageGeneration %in% c('None','withEPackage'))


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
