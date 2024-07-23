plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTable,
                                  datacombined,
                                  prepareElectronicPackage = TRUE){

  # check if all needed library are loaded
  require(tidyr)
  require(data.table)

  # validate Inputs


  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder)

  ## add your own code below are examples how to add headers, figures and tables --------

  # add  section headers
  rmdContainer$addHeader('Concentration time profiles', level = 1)




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
