#' function to generate list of time profiles as facet panels
#'
#' @template projectConfig
#' @param subfolder is name of subfolder in the project/to/path/report directory
#' @param configTableSheet name of sheet in plot configuration xlsx, which defines the plot
#' @template observedData
#' @param nFacetColumns maximal number of facet columns (default 2) used in for facet type "by Order"
#'
#' @return object of class `rmdContainer`
#' @export
plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  dataObserved,
                                  nFacetColumns = 2,
                                  facetAspectRatio = 0.5,
                                  aggregationFlag = c("GeometricStdDev",
                                                      "ArithmeticStdDev",
                                                      "Percentiles",
                                                      "Custom"),
                                  percentiles = c(5, 50, 95),
                                  customFunction = NULL) {
  checkmate::assert_path_for_output(file.path(projectConfiguration$outputFolder, subfolder), overwrite = TRUE)
  checkmate::assertIntegerish(nFacetColumns,lower = 1,len = 1)
  checkmate::assertDouble(facetAspectRatio,lower = 0,len = 1)

  # use data.table format for dataObserved
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  # read aggreagtion function for simulated populations
  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction)


  # read configuration tables
  configTable <- readTimeprofileConfigTable(sheetName = configTableSheet)

  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder
    )

  iRow = 1
  levelLines = which(!is.na(configTable$Level))
  while (iRow <= nrow(configTable)){
    if (!is.na(configTable$Level[iRow])){
      # add  section headers
      rmdContainer$addHeader(configTable$Header[iRow],
                             level = configTable$Level[iRow])
      iRow = iRow +1
    } else{
      # execute plot section
      iEndX = utils::head(which(levelLines>iRow),1)
      if (length(iEndX) ==0 ) {
        iEnd <- nrow(configTable)
      }else{
        iEnd <- levelLines[iEndX]-1
      }

      for (onePlotConfig in split(configTable[seq(iRow,iEnd)], by = "PlotName")) {
        tryCatch(
          {
            rmdContainer <-
              createPanelPlotsForPlotName(projectConfiguration = projectConfiguration,
                                          onePlotConfig = onePlotConfig,
                                          dataObserved = dataObserved,
                                          rmdContainer = rmdContainer,
                                          nFacetColumns = nFacetColumns,
                                          facetAspectRatio = facetAspectRatio,
                                          aggregationFun = aggregationFun
              )
          },
          if (!getOption('OSPSuite.RF.skipFailingPlots', default = FALSE)) {
            stop(err)  # Re-throw the error if "skipFailingPlots" is FALSE
          } else {
            warning(paste('Error during creation of:', onePlotConfig$PlotName[1]))
          }
        )
      }

      iRow <- iEnd + 1

    }

  }

  return(rmdContainer)
}


#' function to generate one plot as facet panels
#'
#' @template projectConfig
#' @template onePlotConfig
#' @template observedDataDT
#' @param rmdContainer object of class `RmdContainer`
#' @param aggregationFun function to aggregate simulated data
#'
#' @return object of class `rmdContainer` with added figures
#' @export
#'
createPanelPlotsForPlotName <- function(projectConfiguration,
                                        onePlotConfig,
                                        dataObserved,
                                        nFacetColumns,
                                        rmdContainer,
                                        facetAspectRatio,
                                        aggregationFun) {

  plotData <- PlotDataTimeProfile$new(projectConfiguration = projectConfiguration,
                                      onePlotConfig = onePlotConfig,
                                      dataObserved = dataObserved,
                                      aggregationFun = aggregationFun,
                                      nFacetColumns = nFacetColumns)

  for (plotType in names(plotTypesTimeprofile())){

    rmdContainer <- generatePlotForPlotType(plotData,
                                            rmdContainer,
                                            facetAspectRatio = facetAspectRatio,
                                            plotType = plotType)

  }

  return(rmdContainer)
}

#' Title
#'
#' @param plotData
#' @param rmdContainer
#' @param facetAspectRatio
#' @param plotType
#'
#' @return
#' @export
generatePlotForPlotType <- function(plotData,
                                    rmdContainer,
                                    facetAspectRatio,
                                    plotType){
  if (!isPlotTypeNeededANdPossible(plotType,plotData)) return(rmdContainer)


  mapSimulatedAndObserved <- getMapSimulatedAndObserved(plotType,plotData)


  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      yLimits = checkAndAdjustYlimits(plotData,
                                      yScale,
                                      timeRangeFilter,
                                      plotType)

      plotObject <-
        switch(plotType,
               TP = ospsuite_plotTimeProfile(
                 plotData = plotData$getDataForTimeRange(timeRangeFilter),
                 yscale = yScale,
                 mapping = getGroupbyMapping(plotData),
                 groupAesthetics = getGroupAesthetics(plotData),
                 geomLLOQAttributes = list(linetype = "dashed"),
                 mapSimulatedAndObserved = mapSimulatedAndObserved,
                 yscale.args = list(limits = yLimits)
               )
        )

      plotObject <- updateGuides(plotData,
                                     plotObject,
                                     plotType)

      # plotObject <- setManualScalevectors(plotData,
      #                                     plotType)

      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio =
                                ifelse(plotType %in% c('PvO','QQ '),
                                       1,
                                       facetAspectRatio)
      )

      # adjust time labels
      if (plotType %in% c('TP','ResvT '))
        plotObject <- plotObject +
        labs(x = plotData$getTimeLabelForTimeRange(timeRangeFilter))

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(plotData,
                                    yScale = yScale,
                                    timeRangeFilter = timeRangeFilter,
                                    plotType = plotType),
        footNoteLines = getFootNoteLines(plotData$getDataForTimeRange(typeFilter = 'observed')),
        figureKey = paste(plotData$configTable$PlotName[1],
                          plotType,
                          ifelse(yScale == 'log','log','linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }




  return(rmdContainer)
}

#' Title
#'
#' @param plotType
#' @param plotData
#'
#' @return
#' @export
isPlotTypeNeededANdPossible <- function(plotType,plotData){
  configColumn = switch(plotType,
                        TP = 'Plot_TimeProfiles',
                        PvO = 'Plot_PredictedVsObserved',
                        ResvT = 'Plot_ResidualsVsTime',
                        ResvO = 'Plot_ResidualsVsObserved',
                        ResH = 'Plot_ResidualsAsHistogram',
                        QQ = 'Plot_QQ',
                        stop(paste('unknown plottype:',plotType)))

  if (plotType == 'TP'){
    return(as.logical(plotData$configTable[[configColumn]][1]))
  } else{
    return(as.logical(plotData$configTable[[configColumn]][1]) &
             plotData$hasObservedData()) # warning is thrown during data preparation
  }

}


getMapSimulatedAndObserved <- function(plotType, plotData){
  if (plotType != 'TP' |
      !plotData$hasObservedData()) return(NULL)

  mapSimulatedAndObserved = data.table(
    simulated = as.character(plotData$data[dataType == 'simulated']$colorIndex) %>% unique(),
    observed = as.character(plotData$data[dataType == 'observed']$colorIndex) %>% unique(),
    color =  plotData$scaleVectors$colour,
    fill =  plotData$scaleVectors$fill
  )

  return(mapSimulatedAndObserved)
}

#' check if Observed data are outside limits and set yLimits for log scale
#'
#' @param yScale y scale of Plot
#' @param plotData object of class PlotDataTimeProfile
#' @param timeRangeFilter Name of time range filter
#'
#' @return yLimits as numeric vector
#' @export
checkAndAdjustYlimits = function(plotData,
                                 yScale,
                                 timeRangeFilter,
                                 plotType) {

  if (plotType != 'TP') return(NULL)

  ylimits <- plotData$configTable[[paste0('ylimit_',yScale)]][1]

  if (is.na(ylimits) || trimws(ylimits) == '') {
    ylimits = NULL
  } else {
    ylimits = eval(parse(text = ylimits))
  }

  simulatedData <- plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'simulated']

  observedData <- plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'observed']
  if (nrow(observedData)> 1)
    observedData <- observedData[yUnit == unique(simulatedData$yUnit)[1]]

  # make sure no observed data are missed by setting y limits
  if (!is.null(ylimits) & nrow(observedData) > 0) {

    if (any(observedData$yValues
            <= ylimits[1]) |
        any(observedData$yValues
            >= ylimits[2],na.rm = TRUE)) {

      stop(paste('data outside ylimit for',plotData$configTable$PlotName[1],
                 'time range', timeRangeFilter))
    }
  }

  # for log scale set always limit to cut very low simulated values at the beginning
  if (is.null(ylimits) & yScale == 'log') {

    # replace Inf and omit first 10% of time range
    timeRangeSim <- range(simulatedData$xValues)
    timeRangeSim[1] <- timeRangeSim[1] + 0.1*diff(timeRangeSim)

    minY <- Inf
    if (nrow(observedData) > 0){
      minY <- min(minY,min(observedData[yValues > 0]$yValues))
      if (any(!is.na(observedData$lloq)))
        minY <- min(minY,min(observedData[!is.na(lloq)]$lloq))
    }

    tmp <- simulatedData[xValues >= timeRangeSim[1]  &
                           xValues <= timeRangeSim[2]]
    if (simulatedData$dataClass[1] == DATACLASS$tpAggregated){
      if (simulatedData$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev){
        tmp = tmp[,yMin := yValues - yErrorValues]
      }
      if (simulatedData$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev){
        tmp = tmp[,yMin := yValues/yErrorValues]
      }
      minY = min(c(minY,
                   tmp[!is.na(yMin) &
                         yMin > 0]$yMin))
    } else{
      minY = min(c(minY,
                   tmp[!is.na(yValues) & yValues > 0]$yValues))
    }

    ylimits =  c(minY/2,NA)
  }

  return(ylimits)

}

#' get Mapping for "groupby" out of column Namesitle
#'
#' @param plotData
#'
#' @return
#' @export
getGroupbyMapping = function(plotData){
  return(eval(parse(text = paste(
    'aes(groupby =', interaction(intersect(
      names(plotData$data), c('colorIndex', 'shapeIndex')
    )), ')'
  )))
  )
}
#' get groupAesthetics out of scaleVactors
#'
#' @param plotData
#'
#' @return vector with aesthtics to group by
#' @export
getGroupAesthetics <- function(plotData){
  return(intersect(
    c('colour', 'fill', 'shape'),
    ggplot2::standardise_aes_names(unique(c(
      names(plotData$scaleVectors),
      names(plotData$scaleVectorsObserved)
    )))
  ))

}
#' Updates Guides for TP
#'
#' @param plotData
#' @param plotType
#'
#' @return
#' @export
updateGuides <- function(plotData,plotObject,plotType){

  if (plotType != 'TP') return(plotObject)

  if (plotData$useColorIndex() & !plotData$hasSimulatedPop()){
    legendTitleSimulated <- paste('simulated', plotData$tpLabelSimulatedMean)
  } else if (plotData$useColorIndex() & plotData$hasSimulatedPop()){
    legendTitleSimulated <- paste0('simulated data\n',
                                   plotData$tpLabelSimulatedMean,
                                   ' with ',plotData$tpLabelSimulatedRange)
  } else {
    legendTitleSimulated <- NULL
  }

  if (plotData$hasObservedData() &
      !plotData$useColorIndex() &
      !plotData$useShapeIndex()){
    legendTitleObserved <- NULL
  } else{
    legendTitleObserved <-  plotData$tpLabelObserved
  }

  aestheticsSuffix <- ifelse(plotData$hasObservedData(),'_ggnewscale_1','')
  guidesList <- setNames(
    lapply(c('colour', 'fill'), function(aesthetic) {
      guide_legend(order = 1, title = legendTitleSimulated)
    }),
    paste0(c('colour', 'fill'), aestheticsSuffix)
  )
  if (plotData$hasObservedData()){
    guidesListObserved <- setNames(
      lapply(c('colour', 'fill'), function(aesthetic) {
        guide_legend(order = 2, title = legendTitleObserved)
      }),
      c('colour', 'fill')
    )
    guidesList <- c(guidesList,
                    guidesListObserved)
  }
  if(plotData$useShapeIndex()){
    if (plotData$useColorIndex()){
      guidesList[[shape]] <- guide_legend(order = 3,title = '')
    } else{
      guidesList[[shape]] <- guide_legend(order = 2,title = legendTitleObserved)
    }
  }

  plotObject <- plotObject + guides(!!!guidesList)

  return(plotObject)
}
#' plot predicted vs observed
#'
#' @inheritParams plotTPPanel
#'
#' @return RmdContainer object with added plot
#' @export
plotPredictedVsObservedPanel <- function(plotData,
                                         rmdContainer){

  foldDistance <- ifelse(!is.na(plotData$configTable$FoldDistance_PvO[1]),
                         as.double(plotData$configTable$FoldDistance_PvO[1]),
                         2)

  mapping <- structure(c(aes(predicted = predicted,
                             observed = yValues),
                         plotData$getGroupbyMapping()), class = "uneval")

  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      plotObject <- ospsuite.plots::plotPredVsObs(
        data = plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'observed'],
        mapping = mapping,
        groupAesthetics = plotData$getGroupAesthetics(),
        xyscale = yScale,
        comparisonLineVector = ospsuite.plots::getFoldDistanceList(foldDistance))

      # add scalings
      for (aesthetic in names(plotData$scaleVectors)){

        plotObject <- plotObject +
          ggplot2:::manual_scale(aesthetic,
                                 values = plotData$scaleVectorsObserved[[aesthetic]],
                                 guide = guide_legend(order = 1,title = NULL))
      }
      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio = 1)

      # adjust labels
      plotObject <- plotObject +
        labs(x = 'observed',
             y = 'predicted')

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(dtCaption = plotData$dtCaption,
                                    yScale = yScale,
                                    timeRangeFilter = plotData$timeRangeTagFilter[[timeRangeFilter]],
                                    plotType = 'PvO'),
        footNoteLines = plotData$getFootNoteLines(),
        figureKey = paste(plotData$configTable$PlotName[1],
                          'PvO',
                          ifelse(yScale == 'log','Log','Linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }

}

#' plot residuals ve Time
#'
#' @inheritParams plotTPPanel
#'
#' @return
#' @export
#'
#' @examples
plotResidualsVsTimePanel <- function(plotData,
                                     rmdContainer,
                                     facetAspectRatio){


  mapping <- structure(c(aes(predicted = predicted,
                             observed = yValues,
                             x = xValues),
                         plotData$getGroupbyMapping()), class = "uneval")

  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      plotObject <- ospsuite.plots::plotResVsCov(
        data = plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'observed'],
        mapping = mapping,
        groupAesthetics = plotData$getGroupAesthetics(),
        residualScale =  yScale)

      # add scalings
      for (aesthetic in names(plotData$scaleVectors)){

        plotObject <- plotObject +
          ggplot2:::manual_scale(aesthetic,
                                 values = plotData$scaleVectorsObserved[[aesthetic]],
                                 guide = guide_legend(order = 1,title = NULL))
      }
      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio = facetAspectRatio)

      plotObject <- plotObject +
        labs(x = plotData$getTimeLabelForTimeRange(timeRangeFilter))

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(dtCaption = plotData$dtCaption,
                                    yScale = yScale,
                                    timeRangeFilter = plotData$timeRangeTagFilter[[timeRangeFilter]],
                                    plotType = 'ResvT'),
        footNoteLines = plotData$getFootNoteLines(),
        figureKey = paste(plotData$configTable$PlotName[1],
                          'ResvT',
                          ifelse(yScale == 'log','Log','Linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }

}

#' plot residuals vs Observed
#'
#' @inheritParams plotTPPanel
#'
#' @return RmdContainer object with added plot
#' @export
plotResidualsVsObservedPanel <- function(plotData,
                                         rmdContainer,
                                         facetAspectRatio){


  mapping <- structure(c(aes(predicted = predicted,
                             observed = yValues,
                             x = yValues),
                         plotData$getGroupbyMapping()), class = "uneval")

  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      plotObject <- ospsuite.plots::plotResVsCov(
        data = plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'observed'],
        mapping = mapping,
        groupAesthetics = plotData$getGroupAesthetics(),
        residualScale =  yScale)

      # add scalings
      for (aesthetic in names(plotData$scaleVectors)){

        plotObject <- plotObject +
          ggplot2:::manual_scale(aesthetic,
                                 values = plotData$scaleVectorsObserved[[aesthetic]],
                                 guide = guide_legend(order = 1,title = NULL))
      }
      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio = facetAspectRatio)

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(dtCaption = plotData$dtCaption,
                                    yScale = yScale,
                                    timeRangeFilter = plotData$timeRangeTagFilter[[timeRangeFilter]],
                                    plotType = 'ResvO'),
        footNoteLines = plotData$getFootNoteLines(),
        figureKey = paste(plotData$configTable$PlotName[1],
                          'ResvO',
                          ifelse(yScale == 'log','Log','Linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }

}


#' plot residuals as histogram
#'
#' @inheritParams plotTPPanel
#'
#' @return RmdContainer object with added plot
#' @export
plotResidualsAsHistogramPanel <- function(plotData,
                                          rmdContainer,
                                          facetAspectRatio){


  mapping <- structure(c(aes(predicted = predicted,
                             observed = yValues),
                         plotData$getGroupbyMapping()), class = "uneval")

  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      plotObject <- ospsuite.plots::plotHistogram(
        data = plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'observed'],
        mapping = mapping,
        residualScale =  yScale,
        distribution = 'normal')

      # add scalings
      for (aesthetic in names(plotData$scaleVectors)){

        plotObject <- plotObject +
          ggplot2:::manual_scale(aesthetic,
                                 values = plotData$scaleVectorsObserved[[aesthetic]],
                                 guide = guide_legend(order = 1,title = NULL))
      }
      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio = facetAspectRatio)

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(dtCaption = plotData$dtCaption,
                                    yScale = yScale,
                                    timeRangeFilter = plotData$timeRangeTagFilter[[timeRangeFilter]],
                                    plotType = 'ResH'),
        footNoteLines = plotData$getFootNoteLines(),
        figureKey = paste(plotData$configTable$PlotName[1],
                          'ResH',
                          ifelse(yScale == 'log','Log','Linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }

}

#' plot residuals as QQ-plot
#'
#' @inheritParams plotTPPanel
#'
#' @return RmdContainer object with added plot
#' @export
plotQQPanel <- function(plotData,
                        rmdContainer){

  mapping <- structure(c(aes(predicted = predicted,
                             observed = yValues),
                         plotData$getGroupbyMapping()), class = "uneval")

  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      plotObject <- ospsuite.plots::plotQQ(
        data = plotData$getDataForTimeRange(timeRangeFilter)[dataType == 'observed'],
        mapping = mapping,
        groupAesthetics = plotData$getGroupAesthetics(),
        residualScale =  yScale)

      # add scalings
      for (aesthetic in names(plotData$scaleVectors)){

        plotObject <- plotObject +
          ggplot2:::manual_scale(aesthetic,
                                 values = plotData$scaleVectorsObserved[[aesthetic]],
                                 guide = guide_legend(order = 1,title = NULL))
      }
      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio = 1)

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(dtCaption = plotData$dtCaption,
                                    yScale = yScale,
                                    timeRangeFilter = plotData$timeRangeTagFilter[[timeRangeFilter]],
                                    plotType = 'QQ'),
        footNoteLines = plotData$getFootNoteLines(),
        figureKey = paste(plotData$configTable$PlotName[1],
                          'QQ',
                          ifelse(yScale == 'log','Log','Linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }

}

#' addFacetColum
#'
#' @param plotObject ggplot object to which the facet should be added
#' @inheritParams createPanePlotsForPlotName
#' @param nFacetColumns maximal number of facet used for facettype byOrder
#'
#' @return ggplot Object with facets
#' @export
addFacets <- function(plotObject,
                      plotData,
                      facetAspectRatio = 0.5) {

  if (!is.null(plotData$nFacetColumns)){


    plotObject <- plotObject +
      facet_wrap(facets = vars(PlotTag),
                 scales = plotData$configTable$FacetScale[1],
                 ncol = plotData$nFacetColumns) +
      theme(aspect.ratio = facetAspectRatio)


  }

  return(plotObject)
}


#' Read the configtable for the timeprofile
#'
#' @param sheetName
#'
#' @return `data.table` with plot configurations
#' @export
readTimeprofileConfigTable <- function(sheetName){
  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  validateConfigTableForTimeProfiles(
    configTable = configTable,
    dataObserved = dataObserved,
    projectConfiguration = projectConfiguration
  )


  plotInputColumns <-
    names(configTable)[grepl("^PlotInputs_", names(configTable))]

  for (col in plotInputColumns){
    configTable[,(col) := as.character(get(col))]
    configTable[is.na(get(col)) & is.na(Level) ,(col):='']
  }

  return(configTable)

}




#' Title
#'
#' @param dtCaption
#' @param yScale
#' @param timeRangeTagFilter
#' @param plotType
#'
#' @return
#' @export
#'
#' @examples
getCaptionForPlot <- function(plotData, yScale, timeRangeFilter, plotType) {

  dtCaption <-
    plotData$dtCaption[eval(parse(text = plotData$timeRangeTagFilter[[timeRangeFilter]]))]


  plotTypeTxt <- switch(plotType,
                        TP = 'Concentration-time profiles',
                        PvO = 'Predicted vs Observed',
                        ResvT = 'Residuals vs time values',
                        ResvO = 'Residuals vs observed values',
                        ResH = 'Residuals distribution ',
                        QQ = 'Residuals as quantile-quantile plot')

  captiontext <- paste(plotTypeTxt,
                       'for',
                       pasteFigureTags(dtCaption, captionColumn = 'outputDisplayName'),
                       'for',
                       pasteFigureTags(dtCaption, captionColumn = 'ScenarioCaptionName'),
                       'on a', ifelse(yScale == 'linear', 'linear', 'logarithmic'),
                       'y-scale.',
                       plotData$configTable$PlotCaptionAddon[1])
  return(captiontext)
}

# auxiliaries --------------
#' @return names list of plot types
plotTypesTimeprofile <- function(){
  c(
    TP = 'TimeProfiles',
    PvO = 'PredictedVsObserved',
    ResvT = 'ResidualsVsTime',
    ResvO = 'Plot_ResidualsVsObserved',
    ResH = 'Plot_ResidualsAsHistogram',
    QQ = 'QQ'
  )
}


# Validation ----------------


#' Validation of config table fo time profiles plots
#'
#' @template projectConfig
#' @param configTable plot Configuration table
#' @template observedDataDT
#'
#' @export
validateConfigTableForTimeProfiles <- function(configTable, dataObserved, projectConfiguration) {
  configTablePlots <- validateHeaders(configTable)

  dtScenarios <- getScenarioDefinitions(projectConfiguration)
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  validateOutputIdsForPlot(dtOutputPaths)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "PlotName",
      "Scenario",
      "ScenarioCaptionName",
      "OutputPathIds",
      "TimeUnit",
      "FacetScale",
      "FacetType",
      "yScale"
    ),
    charactersWithMissing =
      c(
        "DataGroupIds",
        "PlotCaptionAddon",
        "ReferenceScenario"
      ),
    numericColumns = c(
      "TimeOffset",
      "TimeOffset_Reference",
      "FoldDistance_PvO"
    ),
    logicalColumns = c(
      "Plot_TimeProfiles",
      "Plot_PredictedVsObserved",
      "Plot_ResidualsAsHistogram",
      "Plot_ResidualsVsTime",
      "Plot_ResidualsVsObserved",
      "Plot_QQ"
    ),
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("Scenario", "ReferenceScenario"),
        allowedValues = dtScenarios$Scenario_name
      ),
      dataGroupId = list(
        cols = c("DataGroupIds"),
        allowedValues = unique(dataObserved$group)
      ),
      outputPathId = list(
        cols = c("outputPathId"),
        allowedValues = unique(dtOutputPaths$outputPathId)
      ),
      yscale = list(
        cols = c("yscale"),
        allowedValues = c("linear", "log")
      ),
      TimeUnit = list(
        cols = c("TimeUnit"),
        allowedValues = ospsuite::getUnitsForDimension("Time")
      ),
      FacetScale = list(
        cols = c("FacetScale"),
        allowedValues = c("fixed", "free", "free_x", "free_y")
      ),
      FacetType = list(
        cols = c("FacetType"),
        allowedValues = unname(unlist(FACETTYPE))
      )
    )
  )

  validateTimeRangeColumns(configTablePlots)

  validatePanelConsistency(
    configTablePlots = configTablePlots,
    panelColumns = c(
      "TimeUnit",
      "yScale",
      "ylimit_linear",
      "ylimit_log",
      "PlotCaptionAddon",
      "FacetType",
      "FacetScale",
      'Plot_TimeProfiles',
      'Plot_PredictedVsObserved',
      'Plot_ResidualsAsHistogram',
      'Plot_ResidualsVsTime',
      'Plot_ResidualsVsObserved',
      'Plot_QQ'
    ),
    dtOutputPaths = dtOutputPaths
  )

  validatePlotInput(configTablePlots = configTablePlots,
                    plotInputColumns = c('PlotInputs_TP'))

  return(invisible())
}



#' check if panel columns ar filled consistently
#'
#' @template configTablePlots
#' @param panelcolumns vector of columns which should be consistent
validatePanelConsistency <- function(
    configTablePlots,
    panelColumns,
    dtOutputPaths) {
  # Check for unique values of panel columns for each `PlotName`
  uniquePanelValues <-
    configTablePlots[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = PlotName, .SDcols = panelColumns]
  tmp <- lapply(panelColumns, function(col) {
    if (any(uniquePanelValues[[col]] > 1)) stop(paste("values for", col, "should be the same within each panel"))
  })

  # check if more than two different units are combined in one panel
  configTableList <- split(configTablePlots, by = "PlotName")
  for (configPanel in configTableList) {
    outputs <- gsub("[()]", "", splitInputs(configPanel$OutputPathIds))
    if (dplyr::n_distinct(dtOutputPaths[outputPathId %in% outputs]$DisplayUnit) > 2) {
      stop("do not combine more than two yUnits in one Panel")
    }
  }

  return(invisible())
}

#' validates timerange columns
#'
#' Timerange columns must be character and mast contain NA
#' 'total','firstApplication','lastApplication'
#' or a string which evaluates in r to a numeric vector length 2  (e.g. 'c(2,3)' or 'c(2,NA)'
#'
#' @template configTablePlots
validateTimeRangeColumns <- function(configTablePlots) {
  TimeRangeColumns <-
    names(configTablePlots)[grepl("^TimeRange_", names(configTablePlots))]

  if (length(TimeRangeColumns) == 0) stop("You need at least one TimeRange Column")

  validateAtleastOneEntry(configTablePlots, columnVector = TimeRangeColumns)

  validateConfigTablePlots(configTablePlots,
                           charactersWithMissing = TimeRangeColumns
  )

  tryCatch(
    {
      if (!all(sapply(configTablePlots[, ..TimeRangeColumns], function(x) {
        valid <- x %in% c(NA, "total", "firstApplication", "lastApplication")
        if (!all(valid)) {
          tmp <- eval(parse(text = x[!valid]))
          valid <-
            is.numeric(tmp) &&
            length(tmp) == 2 &&
            all(!is.na(tmp))
        }
        return(all(valid))
      }))) {
        stop('invalid inputs in one of the "TimeRange" columns')
      }
    },
    error = function(err) {
      stop('invalid inputs in one of the "TimeRange" columns')
    }
  )

  return(invisible())
}


#' Validates additional Input columns
#'
#' @template configTablePlots
#' @param plotInputColumns vector with names of additional inputs
#'
validatePlotInput <- function(configTablePlots,plotInputColumns){

  for (col in plotInputColumns){
    tryCatch(
      {
        plotInputs <- unique(configTablePlots[!is.na(get(col))][[col]])
        if (length(plotInputs) > 0)
          invisible(lapply(plotInputs, function(x){
            checkmate::assertList(eval(parse(text = paste('list(',x,')'))),names = 'named')})
          )
      },
      error = function(err) {
        stop(paste('invalid inputs in column',col))
      }
    )
  }

}


validateOutputPathIdFormat <- function(configTablePlots) {

  if(any(length(stringr::str_extract_all(configTablePlots$outputPathId, "\\([^)]*$")) > 0) ||
     any(length(stringr::str_extract_all(configTablePlots$outputPathId, "(?<!\\()\\([^)]+")) > 0))
    stop('Please check the brackets in column outputPathId')

}



# support usability --------------------

#' Adds a default sheet to the plot configuration table
#'
#' @template projectConfig
#' @param sheetName name of sheet in plot configuration table
#' @param overwrite `boolean` if TRUE existing configurations will be overwritten
#'
#' @export
addDefaultConfigForTimeProfilePlots <- function(projectConfiguration,
                                                sheetName = "TimeProfile_Panel", overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  if (sheetName %in% wb$sheet_names & !overwrite) {
    stop(paste(sheetName, "already exist"))
  }

  scenarios <- getScenarioDefinitions(projectConfiguration)
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  dtDataGroups <- getDataGroups(projectConfiguration)

  dtNewHeader <- data.table(
    Level = 1,
    Header = "Concentration time profiles"
  )

  dtNewConfig <- data.table(
    PlotName = scenarios$Scenario_name,
    Scenario = scenarios$Scenario_name,
    ScenarioCaptionName = scenarios$Scenario_name,
    OutputPathIds = paste(unique(dtOutputPaths$outputPathId), collapse = ", "),
    TimeUnit = "h",
    TimeOffset = 0,
    TimeOffset_Reference = 0,
    TimeRange_total = TIMERANGE$total,
    TimeRange_firstApplication = TIMERANGE$firstApplication,
    TimeRange_lastApplication = TIMERANGE$lastApplication,
    yScale = "linear, log",
    FacetScale = "fixed",
    FacetType = FACETTYPE[[1]],
    Plot_TimeProfiles = TRUE,
    Plot_PredictedVsObserved = FALSE,
    Plot_ResidualsAsHistogram = FALSE,
    Plot_ResidualsVsTime = FALSE,
    Plot_ResidualsVsObserved = FALSE,
    Plot_QQ = FALSE
  )

  dtNewConfig <- dtNewConfig %>%
    merge(dtDataGroups %>%
            dplyr::select(c('group','DefaultScenario')) %>%
            data.table::setnames(old = c('group','DefaultScenario'),
                                 new = c('DataGroupIds','Scenario')),
          by = 'Scenario',
          all.x = TRUE,sort = FALSE)


  wb <- addConfigToTemplate(
    wb = wb,
    templateSheet = "TimeProfile_Panel",
    sheetName = sheetName,
    dtNewConfig = rbind(dtNewHeader,
                        dtNewConfig,
                        fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
