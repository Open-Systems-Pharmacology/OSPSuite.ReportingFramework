ospsuite_plotForest = function(plotData,
                           vlineIntercept = 1,
                           xscale = 'log',
                           digitsToRound = 2,
                           digitsToShow= 2,
                           tableLabels = c('Ratio',
                                           '90%\nCI lower',
                                           '90%\nCI upper'),
                           xlabel = 'DDI Ratio',
                           textwidthLeft = 0.22,
                           textwidthRight = 0.3){


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

  # y limits
  ymin = min(plotData$yPos) - 0.5
  ymax = -0.5


  # split Window in labels on the left, graphic in the middle and tables on the right
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
    xgridTable = exp(seq(x0,xend,(xend-x0)/7)[c(1,2,4,6,7)])
  } else {
    xend = xlimits[1]
    x0 = xend - textwidthLeft/plotWidth*diff(xlimits)
    xgridLabel = c(x0,xend)

    # get grid for table
    x0 = xlimits[2]
    xend = x0 + textwidthRight/plotWidth*diff(xlimits)
    xgridTable = seq(x0,xend,(xend-x0)/7)[c(1,2,4,6,7)]

  }

  # initial plot with data
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
         y = '')

  plotObject <-
    ospsuite.plots::addXYScale(
      plotObject = plotObject,
      xscale = xscale,
      xscale.args = list(limits = c(xgridLabel[1],xgridTable[5]),
                         expand = expansion(mult = c(0, 0)),
                         breaks = xBreaks,
                         sec.axis = sec_axis(~.,
                                             breaks = xgridTable[c(2,3,4)],
                                             labels = tableLabels)),
      yscale = 'linear',
      yscale.args = list(limits = c(ymin,ymax),
                         expand = expansion(add = c(0, 0)),
                         breaks = plotData[,.(y = median(yPos)),by = displayGroup]$y,
                         labels = levels(plotData$displayGroup))
    )


  plotObject <-  plotObject +
    theme(legend.position = 'none',
          legend.title = element_blank(),
          axis.text.y = element_text(angle = 90,hjust = 0.5,size = 12,face = 'bold'),
          axis.ticks.y = element_line(size = 0),
          axis.text.x.top = element_text(hjust = 0.5,size = 12,face = 'bold'),
          panel.grid = element_blank(),
          aspect.ratio = abs(ymin)/35)


  # add vertical lines
  plotObject <-
    plotObject +
    geom_vline(xintercept = vlineIntercept) +
    geom_vline(xintercept = xlimits, size = 0.3)


  # add y grid
  plotObject <-
    plotObject +
    geom_hline(yintercept = plotData[,.(yGrid = max(yPos)+0.5),by = c('displayName','displayGroup')]$yGrid,
               size = 0.3,linetype = 'dashed') +
    geom_hline(yintercept = plotData[,.(yGrid = max(yPos)+0.5),by = displayGroup]$yGrid,
               size = 0.3,linetype = 'solid')


  # add labels
  plotObject <-
    plotObject +
    geom_text(aes(y = yPosLabel,
                  x = xgridLabel[1],
                  label = displayName),
              hjust = - 0.05)

  plotObject <-
    plotObject +
    geom_text(aes(y = yPos,
                  x =xgridLabel[2],
                  label = parameterLabel),
              hjust = 1.05)

  # add table
  plotObject <-
    plotObject +
    geom_text(aes(
      x = xgridTable[2],
      y = yPos,
      label = formattable::formattable(
        round(x, digitsToRound),
        digits = digitsToShow, format = "f")
    )) +
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


  return(plotObject)


}
