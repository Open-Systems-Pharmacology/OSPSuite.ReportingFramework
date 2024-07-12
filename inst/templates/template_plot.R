#' Template for a plot function called by the Vision::plotRun function
#'
#' @param projectConfig projectConfiguration is mandatory
#' @param resultDirectory path of resultdirectory is mandatory
#' @param ... # instead of ... you can add your own inputs
#'
#' @return object of class rmdContainer, needed for markdown generation
myPlotFunction <- function(projectConfig, resultDirectory, ...) {
  # check if all needed library are loaded
  require(tidyr)
  require(data.table)

  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <- RmdContainer$new()

  ## add your own code below are examples how to add headers, figures and tables --------

  # add  section headers
  rmdContainer$addHeader("My Section", level = 1)
  rmdContainer$addHeader("My Sub Section", level = 2)

  # add a figure
  plotObject <- ggplot(data.table(x = seq(1:3), y = seq(2:4))) +
    geom_point(aes(x = x, y = y))

  rmdContainer$addAndExportFigure(
    plotObject = plotObject,
    path = resultDirectory,
    caption = "My captiontxt",
    figurename = "x_vs_y"
  )

  # add a table
  figurekey <- "quantiles"
  dt <- data.table(
    x = rnorm(1000),
    class = sample(c("Female", "Male"), size = 1000, replace = TRUE)
  ) %>%
    .[, as.list(quantile(x)), by = "class"]



  rmdContainer$addAndExportTable(plotObject,
    path = resultDirectory,
    caption = "My captiontxt",
    tablename = "quantiles"
  )

  return(rmdContainer)
}
