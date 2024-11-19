#' Create a Forest Plot
#'
#' This function generates a forest plot based on the provided data. The plot visualizes the effect sizes (ratios)
#' along with their confidence intervals, allowing for easy comparison across different groups or parameters.
#'
#' @param plotData A data.table containing the data to be plotted. It must include the following columns:
#' \itemize{
#'   \item \code{displayGroup}: Grouping variable for displaying results.
#'   \item \code{displayName}: Name of the display for each row in the plot.
#'   \item \code{parameter}: The parameter for which the effect size is calculated.
#'   \item \code{type}: Type of effect size (e.g., treatment, control).
#'   \item \code{x}: The effect size (e.g., ratio).
#'   \item \code{xmin}: Lower bound of the confidence interval.
#'   \item \code{xmax}: Upper bound of the confidence interval.
#' }
#' @param vlineIntercept Numeric vector for vertical line intercept(s). This can be used to indicate reference lines (e.g., no effect line).
#' Default is an empty vector, which means no vertical lines will be drawn.
#' @param xscale Character string specifying the x-axis scale, either 'linear' or 'log'.
#' Default is 'log', which is often preferred for ratio data.
#' @param digitsToRound Integer indicating the number of digits to round the numerical values in the table. Default is 2.
#' @param digitsToShow Integer indicating the number of digits to display in the table. Default is 2.
#' @param tableLabels Character vector with labels for the table columns. Default includes:
#' \itemize{
#'   \item 'Ratio'
#'   \item '90%\nCI lower'
#'   \item '90%\nCI upper'
#' }
#' @param xlabel Character string for the x-axis label. Default is 'DDI Ratio'.
#' @param textwidthLeft Numeric value for the width of the left text area (0 to 1). Default is 0.22.
#' @param textwidthRight Numeric value for the width of the right text area (0 to 1). Default is 0.3.
#' @param withoutTable Logical indicating whether to display the table on the right side of the plot.
#' If TRUE, the table will not be displayed. Default is FALSE.
#'
#' @return A ggplot object representing the forest plot, which can be further customized or printed.
#' The plot includes error bars for the confidence intervals, points for the effect sizes,
#' and optional tables displaying the numerical values.
#'
#' @export
ospsuite_plotForest = function(plotData,
                           vlineIntercept = c(),
                           xscale = 'log',
                           digitsToRound = 2,
                           digitsToShow= 2,
                           tableLabels = c('Ratio',
                                           '90%\nCI lower',
                                           '90%\nCI upper'),
                           xlabel = 'DDI Ratio',
                           textwidthLeft = 0.22,
                           textwidthRight = 0.3,
                           withoutTable = FALSE){
  checkmate::assertDataTable(plotData)
  checkmate::assertChoice(xscale,c('linear','log'))
  checkmate::assertNames(names(plotData),must.include = c('displayGroup','displayName','parameter','type','x','xmin','xmax'))
  checkmate::assertNumeric(vlineIntercept,null.ok = TRUE)
  checkmate::assertIntegerish(digitsToRound, lower = 0, len = 1)
  checkmate::assertIntegerish(digitsToShow, lower = 0, len = 1)
  checkmate::assertString(xlabel)
  checkmate::assertCharacter(tableLabels)
  checkmate::assertNumeric(textwidthLeft,lower = 0,upper = 1)
  checkmate::assertNumeric(textwidthRight,lower = 0,upper = 1)
  checkmate::assertFlag(withoutTable)

  # set the colorder and calcualte y position of rows
  plotData <- setLabelsAndPosition(plotData)

  # calcultaes xlimits, xBreaks, xgridTable,xgridLabel
  xL <- splitWindowsForTable(
    plotData = plotData,
    textwidthLeft = textwidthLeft,
    textwidthRight = textwidthRight,
    xscale = xscale,
    vlineIntercept = vlineIntercept,
    withoutTable = withoutTable
  )


  # create Plot
  plotObject <- iniForestPlot(plotData,xlabel)

  plotObject <- scaleForestPlot(plotObject =plotObject,
                                plotData = plotData,
                                xscale = xscale ,
                                xgridTable = xL$xgridTable,
                                xgridLabel = xL$xgridLabel,
                                xBreaks = xL$xBreaks,
                                tableLabels = tableLabels,
                                withoutTable = withoutTable)
  plotObject <- plotObject +
    # add vertical lines
    geom_vline(xintercept = vlineIntercept) +
    geom_vline(xintercept = xL$xlimits, size = 0.3) +
    # add y grid
    geom_hline(yintercept = plotData[,.(yGrid = max(yPos)+0.5),by = c('displayName','displayGroup')]$yGrid,
               size = 0.3,linetype = 'dashed') +
    geom_hline(yintercept = plotData[,.(yGrid = max(yPos)+0.5),by = displayGroup]$yGrid,
               size = 0.3,linetype = 'solid') +
    # add labels
    geom_text(aes(y = yPosLabel,
                  x = xL$xgridLabel[1],
                  label = displayName),
              hjust = - 0.05) +
    geom_text(aes(y = yPos,
                  x = xL$xgridLabel[2],
                  label = parameterLabel),
              hjust = 1.05)

  plotObject <- addTableToForest(plotObject = plotObject,
                                 xgridTable = xL$xgridTable,
                                 digitsToRound = digitsToRound,
                                 digitsToShow = digitsToShow,
                                 withoutTable = withoutTable)


  return(plotObject)


}


#' Set Labels and Position for Plot Data
#'
#' This function calculates y-positions and labels for the plot data.
#'
#' @param plotData A data.table containing the data to be plotted.
#'
#' @return A data.table with added columns for y-positions and labels.
#' @keywords internal
setLabelsAndPosition <- function(plotData){
  # get x position for plotting
  plotData %>%
    data.table::setorderv(cols = c('displayGroup','displayName','parameter','type'))
  plotData[,yPos := - .I]
  plotData[,yPosLabel := mean(yPos),by = c('displayName','displayGroup')]
  if (dplyr::n_distinct(plotData$type) > 1 &
      dplyr::n_distinct(plotData$parameter) >1){
    plotData[,parameterLabel := paste(type,parameter)]
  } else if (dplyr::n_distinct(plotData$type) > 1){
    plotData[,parameterLabel := type]
  } else if (dplyr::n_distinct(plotData$parameter) > 1){
    plotData[,parameterLabel := parameter]
  } else{
    plotData[,parameterLabel := '']
  }

  return(plotData)
}



#' Split Windows for Table
#'
#' This function splits the plotting area into labels, graphics, and tables.
#'
#' @param plotData A data.table containing the data to be plotted.
#' @param textwidthLeft Numeric width for the left text area (0 to 1).
#' @param textwidthRight Numeric width for the right text area (0 to 1).
#' @param xscale Character string specifying the x-axis scale, either 'linear' or 'log'.
#' @param vlineIntercept Numeric vector for vertical line intercept(s).
#' @param withoutTable Logical indicating whether to display the table.
#'
#' @return A list containing x-limits, breaks, and grid positions for labels and tables.
#'
#' @keywords internal
splitWindowsForTable <- function(plotData,textwidthLeft,textwidthRight,xscale,vlineIntercept,withoutTable){

  # split Window in labels on the left, graphic in the middle and tables on the right
  if (withoutTable){
    textwidthRight <- 0
    tableIndices <- 1
  } else if('xrange' %in% names(plotData) && any(!is.na(plotData$xrange))){
    tableIndices <- c(1,2,4,5)
  } else{
    tableIndices <- c(1,2,4,6,7)
  }

  plotWidth = 1- textwidthLeft - textwidthRight

  # get xlimits of data and vertical induction and inhibition lines
  if (xscale == 'log'){
    xlimits = c(min(c(plotData[['xmin']]/1.1,vlineIntercept),na.rm = TRUE),
                max(c(plotData[['xmax']]*1.2,vlineIntercept),na.rm = TRUE))

    # get breaks in limits
    xBreaks = scales::breaks_log(n = 7)(xlimits)
  } else {
    offset = (max(plotData[['xmax']]) - min(plotData[['xmin']]))*0.02
    xlimits = c(min(c(plotData[['xmin']] - offset,vlineIntercept),na.rm = TRUE),
                max(c(plotData[['xmax']] + offset,vlineIntercept),na.rm = TRUE))

    # get breaks in limits
    xBreaks = scales::breaks_extended(n = 7)(xlimits)
  }
  xBreaks = sort(unique(c(xBreaks[xBreaks >= xlimits[1] &
                                    xBreaks <= xlimits[2]],
                          vlineIntercept)))

  # get grid for Labels
  if (xscale == 'log'){
    xend = log(xlimits[1])
    x0 = xend - textwidthLeft/plotWidth*diff(log(xlimits))
    xgridLabel = exp(c(x0,xend))

    # get grid for table
    x0 = log(xlimits[2])
    xend = x0 + textwidthRight/plotWidth*diff(log(xlimits))
    xgridTable = exp(seq(x0,xend,(xend-x0)/utils::tail(tableIndices,1))[tableIndices])
  } else {
    xend = xlimits[1]
    x0 = xend - textwidthLeft/plotWidth*diff(xlimits)
    xgridLabel = c(x0,xend)

    # get grid for table
    x0 = xlimits[2]
    xend = x0 + textwidthRight/plotWidth*diff(xlimits)
    xgridTable = seq(x0,xend,(xend-x0)/utils::tail(tableIndices,1))[tableIndices]

  }

  return(list(xlimits = xlimits,
              xBreaks = xBreaks,
              xgridLabel = xgridLabel,
              xgridTable = xgridTable))

}

#' Initialize Forest Plot
#'
#' This function initializes the basic structure of the forest plot.
#'
#' @param plotData A data.table containing the data to be plotted.
#' @param xlabel Character string for the x-axis label.
#'
#' @return A ggplot object representing the initialized forest plot.
#' @keywords internal
iniForestPlot <- function(plotData,xlabel){
  plotObject <-
    ggplot(data = plotData) +
    layerWatermark() +
    geom_errorbarh(aes(y = yPos,
                       xmin = xmin,
                       xmax = xmax,
                       color = type),
                   key_glyph = 'path') +
    geom_point(aes(y = yPos,
                   x = x,
                   fill = type,
                   shape = type),
               size = 4) +
    labs(x = xlabel,
         y = '') +
    theme(legend.position = 'none',
          legend.title = element_blank(),
          axis.text.y = element_text(angle = 90,hjust = 0.5,size = 12,face = 'bold'),
          axis.ticks.y = element_line(size = 0),
          axis.text.x.top = element_text(hjust = 0.5,size = 12,face = 'bold'),
          panel.grid = element_blank())

  return(plotObject)
}

#' Scale Forest Plot
#'
#' This function applies scaling and formatting to the forest plot.
#'
#' @param plotObject A ggplot object representing the forest plot.
#' @param plotData A data.table containing the data to be plotted.
#' @param xscale Character string specifying the x-axis scale, either 'linear' or 'log'.
#' @param xgridLabel Numeric vector for x-axis grid labels.
#' @param xgridTable Numeric vector for x-axis grid table positions.
#' @param xBreaks Numeric vector for x-axis breaks.
#' @param tableLabels Character vector with labels for the table.
#' @param withoutTable Logical indicating whether to display the table.
#'
#' @return A ggplot object with applied scaling.
#'
#' @keywords internal
scaleForestPlot <- function(plotObject,
                            plotData,
                            xscale,
                            xgridLabel,
                            xgridTable,
                            xBreaks,
                            tableLabels,
                            withoutTable){

  # y limits
  ymin = min(plotData$yPos) - 0.5
  ymax = -0.5

  if (withoutTable){
    secAxis <- waiver()
  } else{
    secAxis = sec_axis(~.,
                       breaks = xgridTable[seq(2,length(xgridTable)-1)],
                       labels = tableLabels)
  }

  plotObject <-
    ospsuite.plots::addXYScale(
      plotObject = plotObject,
      xscale = xscale,
      xscale.args = list(limits = c(xgridLabel[1],utils::tail(xgridTable,1)),
                         expand = expansion(mult = c(0, 0)),
                         breaks = xBreaks,
                         sec.axis = secAxis),
      yscale = 'linear',
      yscale.args = list(limits = c(ymin,ymax),
                         expand = expansion(add = c(0, 0)),
                         breaks = plotData[,.(y = median(yPos)),by = displayGroup]$y,
                         labels = levels(plotData$displayGroup))
    ) +
    theme(aspect.ratio = abs(ymin)/35)

}

#' Add Table to Forest Plot
#'
#' This function adds a table to the forest plot if specified.
#'
#' @param plotObject A ggplot object representing the forest plot.
#' @param xgridTable Numeric vector for x-axis grid table positions.
#' @param digitsToRound Integer indicating the number of digits to round to.
#' @param digitsToShow Integer indicating the number of digits to show.
#' @param withoutTable Logical indicating whether to display the table.
#'
#' @return A ggplot object with the table added.
#'
#' @keywords internal
addTableToForest <- function(plotObject,
                             xgridTable,
                             digitsToRound,
                             digitsToShow,
                             withoutTable){

  if (withoutTable) return(plotObject)
  # add table
  plotObject <-
    plotObject +
    geom_text(aes(
      x = xgridTable[2],
      y = yPos,
      label = formattable::formattable(
        round(x, digitsToRound),
        digits = digitsToShow, format = "f")
    ))

  if ( length(xgridTable) == 5){
    plotObject <-
      plotObject +
      geom_text(aes(
      x = xgridTable[3],
      y = yPos,
      label = formattable::formattable(
        round(xmin, digitsToRound),
        digits = digitsToShow,
        format = "f"
      )
    )) +
    geom_text(aes(
      x = xgridTable[4],
      y = yPos,
      label = formattable::formattable(
        round(xmax, digitsToRound),
        digits = digitsToShow,
        format = "f"
      )
    ))
  } else if ( length(xgridTable) == 4){
    plotObject <-
      plotObject +
      geom_text(aes(
        x = xgridTable[3],
        y = yPos,
        label = formattable::formattable(
          round(xrange, digitsToRound),
          digits = digitsToShow,
          format = "f"
        )
      ))
  }

  return(plotObject)

}
