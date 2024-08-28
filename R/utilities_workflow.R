#' Sets options to the values they should have for a valid run
#'
#' relevant options are
#'   - watermark for figures (see vignette(package = 'ospsuite.plots',topic = 'ospsuite_plots'))
#'   - OSPSuite.RF.skipFailingPlots
#'
#'
#' @param isValidRun  if TTRUE
#'
#' @export
executeAsValidRun <- function(isValidRun) {
  # set options to enable watermarks
  setOspsuite.plots.option(
    optionKey = OptionKeys$watermark_enabled,
    value = !isValidRun
  )

  # skip failures in figure generation
  options(OSPSuite.RF.skipFailingPlots = !isValidRun)

  return(invisible())
}
