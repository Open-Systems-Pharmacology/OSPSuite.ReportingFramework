#' Template for a plot function called by the Vision::plotRun function
#'
#' @param projectConfiguration projectConfiguration
#' @param subfolder is name of subfolder in the project/to/path/report directory
#'    each function should have its own subfolder, so each function is responsible to keep figureKeys unique within this folder
#' @param ... # instead of ... you can add your own inputs
#'
#' @return object of class rmdContainer, needed for markdown generation
myPlotFunction <- function(projectConfiguration, subfolder, ...) {
  # check if all needed library are loaded
  require(tidyr)
  require(data.table)

  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder)

  ## add your own code below are examples how to add headers, figures and tables --------

  # add  section headers
  rmdContainer$addHeader("My Section", level = 1)
  rmdContainer$addHeader("My Sub Section", level = 2)

  # add a figure
  plotObject <-
    ospsuite.plots::plotHistogram(
      data = data.frame(x = rnorm(100)),
      mapping = ggplot2::aes(x = x))

  rmdContainer$addAndExportFigure(
    plotObject = plotObject,
    caption = "My captiontxt",
    footNoteLines = NULL,
    figureKey = "myHistogram"
  )

  # add a table
  figurekey <- "quantiles"
  dt <- data.table(
    x = rnorm(1000),
    class = sample(c("Female", "Male"), size = 1000, replace = TRUE)
  ) %>%
    .[, as.list(quantile(x)), by = "class"]

  # change digits of significance from 3 (default) to 4
  rmdContainer$digitsOfSignificance <- 4

  rmdContainer$addAndExportTable(dt = dt,
    path = resultDirectory,
    caption = "My captiontxt",
    tablename = "quantiles"
  )

  return(rmdContainer)
}
