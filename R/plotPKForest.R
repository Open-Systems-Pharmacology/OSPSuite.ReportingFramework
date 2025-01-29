plotPKForestAbsoluteValuesWithVariance <- function(projectConfiguration,
                                                   onePlotConfig,
                                                   pkParameterDT,
                                                   pkParameterObserved = NULL,
                                                   aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles"),
                                                   percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1,3,5)],
                                                   scaleVectors = list(
                                                     simulated = list(
                                                       color = 'darkgrey',
                                                       fill = 'darkgrey',
                                                       shape = 'circle'
                                                     ),
                                                     observed = list(
                                                       color = 'darkgrey',
                                                       fill = 'lightgrey',
                                                       shape = 'triangle filled'
                                                     )
                                                   ),
                                                   labelWrapWidth = 10,
                                                   vlineIntercept = NULL,
                                                   ...){
  validateCommonInputs(pkParameterDT = pkParameterDT,
                       pkParameterObserved = pkParameterObserved,
                       scaleVectors = scaleVectors,
                       labelWrapWidth = labelWrapWidth,
                       vlineIntercept = vlineIntercept)

  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles)

  plotList <- plotPKForest(projectConfiguration,
                           onePlotConfig = onePlotConfig,
                           pkParameterDT = pkParameterDT,
                           ratioMode = 'none',
                           asCI = FALSE,
                           pkParameterObserved = NULL,
                           aggregationFlag = aggregationFlag,
                           aggregationFun = aggregationFun,
                           percentiles = percentiles,
                           scaleVectors = scaleVectors,
                           vlineIntercept = vlineIntercept,
                           labelWrapWidth = labelWrapWidth,
                           ...)
  return(plotList)
}
plotPKForestAbsoluteValuesWithCI <- function(projectConfiguration,
                                             onePlotConfig,
                                             pkParameterDT,
                                             pkParameterObserved = NULL,
                                             coefficientOfVariation = NULL,
                                             nObservationDefault,
                                             nBootstrap = 1000,
                                             statFun = c('geometric mean' = function(x) exp(mean(log(x[x>0])))),
                                             confLevel = 0.9,
                                             scaleVectors = list(
                                               simulated = list(
                                                 color = 'darkgrey',
                                                 fill = 'darkgrey',
                                                 shape = 'circle'
                                               ),
                                               observed = list(
                                                 color = 'darkgrey',
                                                 fill = 'lightgrey',
                                                 shape = 'triangle filled'
                                               )
                                             ),
                                             labelWrapWidth = 10,
                                             digitsToRound = 2,
                                             digitsToShow = 2,
                                             vlineIntercept = NULL,
                                             ...){
  ratioMode = 'none'

  validateCommonInputs(pkParameterDT = pkParameterDT,
                       pkParameterObserved = pkParameterObserved,
                       scaleVectors = scaleVectors,
                       labelWrapWidth = labelWrapWidth,
                       vlineIntercept = vlineIntercept)

  validateBootstrappingInputs(nObservationDefault = nObservationDefault,
                              nBootstrap = nBootstrap,
                              confLevel = confLevel,
                              coefficientOfVariation = coefficientOfVariation,
                              statFun = statFun,
                              ratioMode = ratioMode)


  plotList <- plotPKForest(projectConfiguration = projectConfiguration,
                           onePlotConfig = onePlotConfig,
                           pkParameterDT = pkParameterDT,
                           pkParameterObserved = pkParameterObserved,
                           ratioMode = ratioMode,
                           asCI = TRUE,
                           aggregationFlag = NULL,
                           aggregationFun = statFun,
                           percentiles = NULL,
                           coefficientOfVariation = coefficientOfVariation,
                           nObservationDefault = nObservationDefault,
                           nBootstrap = nBootstrap,
                           confLevel = confLevel,
                           vlineIntercept = vlineIntercept,
                           scaleVectors = scaleVectors,
                           digitsToRound =digitsToRound,
                           digitsToShow = digitsToShow,
                           labelWrapWidth = labelWrapWidth,
                           ...)


  return(plotList)
}
plotPKForestRatiosWithVariance <- function(projectConfiguration,
                                           onePlotConfig,
                                           pkParameterDT,
                                           pkParameterObserved = NULL,
                                           aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles"),
                                           percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1,3,5)],
                                           scaleVectors = list(
                                             simulated = list(
                                               color = 'darkgrey',
                                               fill = 'darkgrey',
                                               shape = 'circle'
                                             ),
                                             observed = list(
                                               color = 'darkgrey',
                                               fill = 'lightgrey',
                                               shape = 'triangle filled'
                                             )
                                           ),
                                           labelWrapWidth = 10,
                                           vlineIntercept = NULL,
                                           ...){
  validateCommonInputs(pkParameterDT = pkParameterDT,
                       pkParameterObserved = pkParameterObserved,
                       scaleVectors = scaleVectors,
                       labelWrapWidth = labelWrapWidth,
                       vlineIntercept = vlineIntercept)

  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles)

  ratioMode <- getRatioMode(onePlotConfig = onePlotConfig,
                            pkParameterDT = pkParameterDT,
                            asRatio = TRUE)

  plotList <- plotPKForest(projectConfiguration,
                           onePlotConfig = onePlotConfig,
                           pkParameterDT = pkParameterDT,
                           ratioMode = ratioMode,
                           asCI = FALSE,
                           pkParameterObserved = pkParameterObserved,
                           aggregationFlag = aggregationFlag,
                           aggregationFun = aggregationFun,
                           percentiles = percentiles,
                           scaleVectors = scaleVectors,
                           vlineIntercept = vlineIntercept,
                           labelWrapWidth = labelWrapWidth,
                           ...)


  return(plotList)
}
plotPKForestRatiosWithCI <- function(projectConfiguration,
                                     onePlotConfig,
                                     pkParameterDT,
                                     pkParameterObserved = NULL,
                                     coefficientOfVariation = NULL,
                                     nObservationDefault,
                                     nBootstrap = 1000,
                                     statFun = c('geometric mean' = function(x) exp(mean(log(x[x>0])))),
                                     confLevel = 0.9,
                                     scaleVectors = list(
                                       simulated = list(
                                         color = 'darkgrey',
                                         fill = 'darkgrey',
                                         shape = 'circle'
                                       ),
                                       observed = list(
                                         color = 'darkgrey',
                                         fill = 'lightgrey',
                                         shape = 'triangle filled'
                                       )
                                     ),
                                     labelWrapWidth = 10,
                                     digitsToRound = 2,
                                     digitsToShow = 2,
                                     vlineIntercept = c(1),
                                     ...){
  ratioMode <- getRatioMode(onePlotConfig = onePlotConfig,
                            pkParameterDT = pkParameterDT,
                            asRatio = TRUE)

  validateBootstrappingInputs(nObservationDefault = nObservationDefault,
                              nBootstrap = nBootstrap,
                              confLevel = confLevel,
                              coefficientOfVariation = coefficientOfVariation,
                              statFun = statFun,
                              ratioMode = ratioMode)

  plotList <- plotPKForest(projectConfiguration = projectConfiguration,
                           onePlotConfig = onePlotConfig,
                           pkParameterDT = pkParameterDT,
                           pkParameterObserved = pkParameterObserved,
                           ratioMode = ratioMode,
                           asCI = TRUE,
                           aggregationFlag = NULL,
                           aggregationFun = statFun,
                           percentiles = NULL,
                           coefficientOfVariation = coefficientOfVariation,
                           nObservationDefault = nObservationDefault,
                           nBootstrap = nBootstrap,
                           confLevel = confLevel,
                           vlineIntercept = vlineIntercept,
                           scaleVectors = scaleVectors,
                           digitsToRound =digitsToRound,
                           digitsToShow = digitsToShow,
                           labelWrapWidth = labelWrapWidth,
                           ...)



  return(plotList)
}


plotPKForest <- function(projectConfiguration,
                         onePlotConfig,
                         pkParameterDT,
                         pkParameterObserved,
                         ratioMode,
                         asCI,
                         aggregationFlag = NULL,
                         aggregationFun,
                         percentiles = NULL,
                         coefficientOfVariation = NA,
                         nObservationDefault = NA,
                         nBootstrap = NA,
                         confLevel = NA,
                         vlineIntercept,
                         scaleVectors,
                         digitsToRound = NA,
                         digitsToShow = NA,
                         labelWrapWidth = NA,
                         ...) {
  # separates the comma separate inputs to rows
  onePlotConfig <- separateAndTrim(data = onePlotConfig,columnName = 'outputPathIds') %>%
    separateAndTrim(columnName = 'pkParameters')
browser()
  # switch to naming convention of forest plot, filter for relevant data and calculate ratio
  pkParameterObserved <- filterParameterObserved(pkParameterObserved,onePlotConfig)

  pkParameterDT <-
    filterParameterSimulated(
      projectConfiguration = projectConfiguration,
      pkParameterDT = pkParameterDT,
      onePlotConfig = onePlotConfig,
      ratioMode = ratioMode,
      asCI = asCI,
      coefficientOfVariation = coefficientOfVariation
    )

  # merge observedData
  pkParameterDT[,nObservations := nObservationDefault]

  plotDataGroup <- prepareDataForPKForest(
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    ratioMode = ratioMode,
    asCI = asCI,
    aggregationFun = aggregationFun,
    nBootstrap = nBootstrap,
    confLevel = confLevel
  )

  plotList <- list()

  for (groupName in names(plotDataGroup)){

    columnList <-
      getColumnSelectionForPKForest(plotData = plotDataGroup[[groupName]], ratioMode = ratioMode)

    tableLabels <-
      getTableLabelsForPKForest(
        plotData = plotDataGroup[[groupName]],
        percentiles = percentiles,
        asCI = asCI,
        confLevel = confLevel,
        ratioMode = ratioMode
      )

    for (xScale in splitInputs(onePlotConfig$xScale[1])){
      combinedObject <-
        plotForest(plotData = plotDataGroup[[groupName]],
                   yColumn = columnList$yColumn,
                   xLabel = columnList$xLabel,
                   yFacetColumns = columnList$yFacetColumns,
                   xFacetColumn = columnList$xFacetColumn,
                   xscale = xScale,
                   xscale.args = getXorYlimits(onePlotConfig = onePlotConfig,
                                               xOryScale = xScale,
                                               direction = 'x',
                                               ...),
                   tableColumns = names(tableLabels),
                   tableLabels = tableLabels,
                   labelWrapWidth = labelWrapWidth,
                   digitsToRound = digitsToRound,
                   digitsToShow = digitsToShow,
                   withTable = is.null(columnList$xFacetColumn) & asCI)

      combinedObject$plotObject <-
        adjustPkForestPlotObject(plotObject = combinedObject$plotObject,
                                 scaleVectors = scaleVectors,
                                 vlineIntercept = vlineIntercept)


      # Prepare for export
      setattr(combinedObject, 'caption',
              getCaptionForForestPlot(plotData = plotDataGroup[[groupName]],
                                      xScale = xScale,
                                      plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
                                      ratioMode = ratioMode,
                                      asCI = asCI))
      setattr(combinedObject,'footNoteLines',
              getFootnoteLinesForForrestPlots(
                plotData = plotDataGroup[[groupName]],
                ratioMode = ratioMode,
                asCI = asCI,
                dtDataReference = NULL
              )
      )

      # Create figure key and store plot
      figureKey <- paste(onePlotConfig$plotName[1],
                         groupName,
                         ifelse(xScale == "log", "log", "linear"),
                         ifelse(ratioMode == 'none','abs', 'ratio'),
                         ifelse(asCI, 'CI', 'Variance'),
                         sep = "-")
      plotList[[figureKey]] <- combinedObject

    }

  }

  return(plotList)
}
# auxiliary  -------------


prepareDataForPKForest <- function(
    onePlotConfig,
    pkParameterDT,
    ratioMode,
    asCI,
    aggregationFun,
    nBootstrap,
    confLevel){

  switch(ratioMode,
         'none' = {
           if (asCI){
             plotData <- pkParameterDT[,as.list(getCIbyBootstrapping(y = value,
                                                                     statFun = aggregationFun,
                                                                     scenarioName = scenario,
                                                                     pkParameter = pkParameter,
                                                                     outputPathId = outputPathId,
                                                                     nObservations = nObservations,
                                                                     nBootstrap = nBootstrap,
                                                                     confLevel = confLevel)),
                                       by = c('pkParameter','outputPathId','scenario')]
           } else {
             plotData <- getAggregatedVariance(dt = pkParameterDT,
                                               aggregationFun = aggregationFun,
                                               valueColumn = 'value',
                                               identifier = c('pkParameter','outputPathId','scenario'))
           }
         },
         'individualRatios' = {
           if (asCI){
             plotData <- pkParameterDT[,as.list(getCIbyBootstrapping(y = ratio,
                                              statFun = aggregationFun,
                                              scenarioName = scenario,
                                              pkParameter = pkParameter,
                                              outputPathId = outputPathId,
                                              nObservations = nObservations,
                                              nBootstrap = nBootstrap,
                                              confLevel = confLevel)),
                                       by = c('pkParameter','outputPathId','scenario','referenceScenario')]
           } else {
             plotData <- getAggregatedVariance(dt = pkParameterDT,
                                               aggregationFun = aggregationFun,
                                               valueColumn = 'ratio',
                                               identifier = c('scenario','pkParameter','outputPathId','referenceScenario'))
           }
         },
         'ratioOfPopulation' = {
           plotData <- data.table()
         }

  )
  plotData[,dataType := 'simulated']

  # add descriptions
  plotData <- merge(plotData,
                    onePlotConfig[,c('scenario','scenarioShortName','scenarioGroup')] %>%
                      unique(),
                    by = 'scenario')

  plotData$scenarioShortName <-
    factor(
      plotData$scenarioShortName,
      levels <- unique(onePlotConfig$scenarioShortName),
      ordered = TRUE
    )

  plotData <- merge(plotData,
                    configEnv$outputPaths[,c('outputPathId','displayNameOutputs', 'displayUnit')] %>%
                      unique(),
                    by = 'outputPathId')


  plotData$outputPathId <-
    factor(
      plotData$outputPathId,
      levels <- configEnv$outputPaths[outputPathId %in% plotData$outputPathId,]$outputPathId,
      ordered = TRUE
    )

  plotData[,plotTag := generatePlotTag(as.numeric(outputPathId))]


  plotData <- plotData  %>%
    merge(pkParameterDT[,c('pkParameter','displayNamePKParameter','displayUnitPKParameter')] %>%
            unique(),
          by = c('pkParameter'))
  plotData$displayNamePKParameter <-
    factor(
      plotData$displayNamePKParameter,
      levels = unique(pkParameterDT[pkParameter %in% plotData$pkParameter]$displayNamePKParameter),
      ordered = TRUE
    )

  if (ratioMode == 'none'){
    plotDataGroup <- split(plotData,by = 'pkParameter')
  } else {
    plotDataGroup <- split(plotData,by = 'outputPathId')
  }

  return(plotDataGroup)
}


filterParameterObserved <- function(pkParameterObserved,onePlotConfig){
  if (is.null(pkParameterObserved)) return(NULL)

  pkParameterObserved <- data.table::setnames(x = pkParameterObserved,
                                              old = c('values','minValue','maxValue','numberOfIndividuals','parameter','errorValues'),
                                              new = c('x', 'xMin', 'xMax', 'numberOfObservations','pkParameter', 'xErrorValues'),
                                              skip_absent = TRUE)

  pkParameterObserved <- pkParameterObserved %>%
    merge(onePlotConfig[,c('dataGroupId','pkParameter','outputPathId')] %>%
            unique(),
          by.x = c('group','pkParameter','outputPathId'),
          by.y = c('dataGroupId','pkParameter','outputPathId'))

  return(pkParameterObserved)
}
filterParameterSimulated <- function(projectConfiguration,pkParameterDT,onePlotConfig,ratioMode,coefficientOfVariation,asCI){

  switch(ratioMode,
         'none' = {
           pkParameterDT <- merge(onePlotConfig[,c('pkParameter','outputPathId','scenario')] %>%
                                    unique(),
                                  pkParameterDT,
                                  by.x = c('scenario','pkParameter','outputPathId'),
                                  by.y = c('scenarioName','parameter','outputPathId'))
         },
         'individualRatios' = {
           pkParameterDT <-
             merge(onePlotConfig[!is.na(referenceScenario),
                                 c('pkParameter','outputPathId','scenario','referenceScenario')] %>%
                     unique(),
                   pkParameterDT,
                   by.x = c('scenario','pkParameter','outputPathId'),
                   by.y = c('scenarioName','parameter','outputPathId')) %>%
             merge(pkParameterDT[,c('scenarioName','parameter','outputPathId','individualId','value','displayUnitPKParameter')],
                   by.x = c('referenceScenario','pkParameter','outputPathId','individualId'),
                   by.y = c('scenarioName','parameter','outputPathId','individualId'),
                   suffixes = c('','.reference'))
           tmp <-
             pkParameterDT[displayUnitPKParameter != displayUnitPKParameter.reference, c(
               'scenario',
               'referenceScenario',
               'pkParameter',
               'outputPathId',
               'displayUnitPKParameter',
               'displayUnitPKParameter.reference'
             )] %>%  unique()
           if (nrow(tmp) > 0){
             print(tmp)
             stop('Units are not consistent between scenario and referenceScenario')
           }
           pkParameterDT[,displayUnitPKParameter.reference := NULL]

           if (ratioMode == 'individualRatios' & asCI){
             sigma <- sqrt(log(coefficientOfVariation^2 + 1))
             pkParameterDT[, valueIOV := addInterOccasionalVariability(value = value,
                                                                       scenarioName = scenario,
                                                                       pkParameter = pkParameter,
                                                                       outputPathId = outputPathId,
                                                                       sigma = sigma),
                           by = .(scenario, pkParameter, outputPathId)]
             pkParameterDT[, valueIOV.reference := addInterOccasionalVariability(value = value.reference,
                                                                                 scenarioName = referenceScenario,
                                                                                 pkParameter = pkParameter,
                                                                                 outputPathId = outputPathId,
                                                                                 sigma = sigma),
                           by = .(referenceScenario, pkParameter, outputPathId)]

             pkParameterDT[,ratio := valueIOV/valueIOV.reference]

             exportPKWithIOVForEPackage(projectConfiguration,pkParameterDT)

           } else {
             pkParameterDT[,ratio := value/value.reference]
           }
         },
         'ratioOfPopulation' = {
           pkParameterDT <- rbind(
             merge(onePlotConfig[!is.na(referenceScenario),
                                 c('pkParameter','outputPathId','scenario')] %>%
                     unique(),
                   pkParameterDT,
                   by.x = c('scenario','pkParameter','outputPathId'),
                   by.y = c('scenarioName','parameter','outputPathId')) %>%
               dplyr::mutate(scenarioType = 'default'),
             merge(onePlotConfig[!is.na(referenceScenario),
                                 c('pkParameter','outputPathId','referenceScenario')] %>%
                     unique(),
                   setnames(old = 'referenceScenario',new = 'scenario'),
                   pkParameterDT,
                   by.x = c('scenario','pkParameter','outputPathId'),
                   by.y = c('scenarioName','parameter','outputPathId')) %>%
               dplyr::mutate(scenarioType = 'reference'))
         }
  )

  return(pkParameterDT)
}

getTableLabelsForPKForest <- function(plotData,percentiles,asCI,confLevel,ratioMode){

  if (asCI){
    tableColumns <- c('x','xMin','xMax')
    tableLabels = c(ifelse(ratioMode == 'none','Value','Ratio'),
                    paste0(confLevel*100,'%\nCI lower'), paste0(confLevel*100,'90%\nCI upper'))
  } else {
    if (plotData$xErrorType[1] %in% c(ospsuite::DataErrorType$GeometricStdDev,
                                      ospsuite::DataErrorType$ArithmeticStdDev)){
      tableColumns <- c('x','xErrorValues')
      tableLabels <- getErrorLabels(plotData$xErrorType[1]) %>%
        gsub(pattern = 'geometric ',replacement = 'geometric\n')

    } else {
      tableColumns <- c('x','xMin','xMax')
      tableLabels <- formatPercentiles(percentiles[c(2,1,3)], "\npercentile")

    }
  }

  names(tableLabels) <- tableColumns

  return(tableLabels)

}

getColumnSelectionForPKForest <- function(plotData,ratioMode){
  columnList = list()
  if(ratioMode == 'none'){
    columnList$yColumn <- 'scenarioShortName'
    columnList$yFacetColumns <- c('scenarioGroup')
    columnList$xLabel <- plotData$displayNamePKParameter[1]
    pkUnit <- plotData$displayUnitPKParameter[1]
    if (pkUnit != '') columnList$xLabel <- paste0(columnList$xLabel ,' [',pkUnit,']')
  } else {
    columnList$yColumn <- 'displayNamePKParameter'
    columnList$yFacetColumns <- c('scenarioGroup','scenarioShortName')
    columnList$xLabel <- 'Ratio'
  }

  if (dplyr::n_distinct(plotData$plotTag) > 1){
    columnList$xFacetColumn = 'plotTag'
  } else {
    columnList$xFacetColumn = NULL
  }
  return(columnList)
}
getAggregatedVariance <- function(dt,
                                  aggregationFun,
                                  valueColumn,
                                  identifier) {
  # Do Aggregation

  dtAggregated <- dt[, as.list(aggregationFun(get(valueColumn))), by = identifier]

  if (!is.null(dtAggregated$yErrorValues)){
    if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
      dtAggregated[,yMin := yValues - yErrorValues]
      dtAggregated[,yMax := yValues + yErrorValues]
    }
    if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
      dtAggregated[,yMin := yValues/yErrorValues]
      dtAggregated[,yMax := yValues*yErrorValues]
    }
  }

  data.table::setnames(dtAggregated,old = c('yMin','yValues','yMax','yErrorValues','yErrorType'),
                       new = c('xMin', 'x', 'xMax','xErrorValues','xErrorType'),
                       skip_absent = TRUE)



  return(dtAggregated)
}

adjustPkForestPlotObject <- function(plotObject,scaleVectors,vlineIntercept){
  plotObject <- plotObject +
    theme(legend.title = element_blank()) +
    scale_color_manual(values = unlist(lapply(scaleVectors, getElement, 'color'))) +
    scale_fill_manual(values = unlist(lapply(scaleVectors, getElement, 'color'))) +
    scale_shape_manual(values = unlist(lapply(scaleVectors, getElement, 'shape')))

  if (!is.null(vlineIntercept)){
    plotObject <- plotObject +
      geom_vline(xintercept = vlineIntercept)
  }
  if (dplyr::n_distinct(plotObject$data$dataType) == 1){
    plotObject <- plotObject +
      theme(legend.position = 'none')
  } else {
    plotObject <- plotObject +
      theme(legend.position = 'bottom')
  }

  return(plotObject)
}

addInterOccasionalVariability <- function(value, scenarioName, pkParameter, outputPathId,sigma) {

  # Set seed
  setSeedForPKParameter(scenarioName, pkParameter, outputPathId)
  valueIOV = value * exp(rnorm(n = length(value), mean = 0, sd = sigma))

  return(valueIOV) # Return the new value
}


setSeedForPKParameter <- function(scenarioName, pkParameter, outputPathId){
  seedString <- paste(scenarioName[1], pkParameter[1], outputPathId[1],sep = "_")
  numericSeed <- sum(utf8ToInt(seedString))
  set.seed(numericSeed)
  return(invisible())
}

getCIbyBootstrapping <- function(y,
                     statFun = aggregationFun,
                     scenarioName,
                     pkParameter,
                     outputPathId,
                     nObservations,
                     nBootstrap,
                     confLevel){

  setSeedForPKParameter(scenarioName, pkParameter, outputPathId)
  y <- y[!is.na(y)]
  nObservations <- nObservations[1]

  returnList <- list(x = statFun[[1]](y),
                     xErrorType = paste0(names(statFun),'|',100*confLevel,'% confidence interval'))

  if (length(y) < nObservations) {
    warning(paste('not enough simulated values for boostrapping',
                                            scenarioName, pkParameter, outputPathId))

    return(c(returnList,
             list(
                xMin = NA,
                xMax = NA)))
  }


  bstrap <-   replicate(nBootstrap,
                        statFun[[1]](sample(x = y, size = nObservations, replace = FALSE)),
                        simplify = TRUE)

  return(c(returnList,
           list(xMin = quantile(bstrap, probs = (1 - confLevel) / 2,names = FALSE),
     xMax = quantile(bstrap, probs = 1 - ((1 - confLevel) / 2),names = FALSE))))


}

getCaptionForForestPlot <- function(plotData,
                                    xScale,
                                    plotCaptionAddon,
                                    ratioMode,
                                    asCI){
  dtCaption <-
    plotData[, c(
      'displayNameOutputs',
      'plotTag'
    )] %>%  unique()

  if (ratioMode == 'none'){
    pktext <- concatWithAnd(unique(plotData$displayNamePKParameter))
  } else {
    pktext <- concatWithAnd(paste(unique(plotData$displayNamePKParameter), 'ratios'))
  }

  captiontxt = paste0('Simulated ',
                      ifelse('observed' %in% unique(plotData$type), ' and observed ', ''),
                      pktext,
                      " of ",
                      pasteFigureTags(dtCaption, captionColumn = "displayNameOutputs"),
                      " on a ", ifelse(xScale == "linear", "linear", "logarithmic"),
                      " x-scale."
  )

  if (!is.na(plotCaptionAddon) & plotCaptionAddon !=''){
    captiontxt = paste(captiontxt,plotCaptionAddon)
  }


}

getFootnoteLinesForForrestPlots <- function(plotData,ratioMode,asCI,dtDataReference){

  errorLabels <- getErrorLabels(plotData$xErrorType[1])

  footnoteLines <-
    paste0('Simulated ',
           ifelse('observed' %in% unique(plotData$type), ' and observed ', ''),
           "data is displayed as ",
           paste(errorLabels, collapse = " and "),
           '.')
  if (ratioMode == 'ratioOfPopulation'){
    footnoteLines <- c(footnoteLines,
                       'Ratios were calculated as ratios of population summary statistics.')
  }

  # # filter used data
  # if (nrow(dtDataReference) > 0) {
  #   footnoteLines <- c(
  #     footnoteLines,
  #     paste0(
  #       "Data source: [",
  #       paste(
  #         dtDataReference$reference %>%
  #           unique(),
  #         collapse = ", "
  #       ),
  #       "]   "
  #     )
  #   )
  # }

  return(footnoteLines)

}

# validation ---------
validatePKForestAbsoluteValuesWithVarianceConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)
    return(invisible())
  }
validatePKForestAbsoluteValuesWithCIConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)
    return(invisible())
  }
validatePKForestRatiosWithVarianceConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)

    # check if reference Scenarios are there
    tmp <- configTablePlots[,.(isValid = any(!is.na(referenceScenario))),by = plotName]
    if (any(tmp$isValid == FALSE)){
      stop(paste('For ratio plots at lease one reference scenario has to be selected. Check PlotName',paste(tmp$plotName,collapse = ', ')))
    }
    return(invisible())
  }
validatePKForestRatiosWithCIConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)

    # check if reference Scenarios are there
    tmp <- configTablePlots[,.(isValid = any(!is.na(referenceScenario))),by = plotName]
    if (any(tmp$isValid == FALSE)){
      stop(paste('For ratio plots at lease one reference scenario has to be selected. Check PlotName',paste(tmp$plotName,collapse = ', ')))
    }
    return(invisible())
  }

#' Validate PK Forest Configuration Table
#'
#' Validates the configuration table for PK forest plots.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#'
#' @keywords internal
validatePKForestConfigTable <- function(configTable, pkParameterDT,...) {

  configTablePlots <- validateHeaders(configTable)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("plotName","scenarioShortName","scenario"),
    charactersWithMissing = c("scenarioGroup","plotCaptionAddon",'dataGroupId'),
    logicalColumns = NULL,
    numericRangeColumns = c("xlimit_linear", "xlimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("scenario", "referenceScenario"),
        allowedValues = unique(pkParameterDT$scenarioName)
      ),
      pkParameter = list(
        cols = c("pkParameters"),
        allowedValues = unique(pkParameterDT$parameter)
      ),
      outputPathId = list(
        cols = c("outputPathIds"),
        allowedValues = unique(pkParameterDT$outputPathId)
      )
    )
  )

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "plotCaptionAddon",
      "outputPathIds")
  )

  return(configTablePlots)
}

validateBootstrappingInputs <- function(nObservationDefault,nBootstrap,confLevel,coefficientOfVariation,statFun,ratioMode){
  checkmate::assertIntegerish(nObservationDefault, lower = 0, len = 1)
  checkmate::assertIntegerish(nBootstrap, lower = 0, len = 1)
  checkmate::assertDouble(confLevel, lower = 0, upper = 1, len = 1, finite = TRUE)
  checkmate::assertFunction(statFun[[1]])
  checkmate::assertNamed(statFun)


  if (ratioMode == 'individualRatios'){
    checkmate::assertDouble(coefficientOfVariation, lower = 0, len = 1, finite = TRUE)
  } else {
    if (!is.null(coefficientOfVariation))
      warning('Ratio is calculated as ratio of different populations, inter occasional variability will be therefore neglected')
  }

}

validateCommonInputs <- function(pkParameterDT,
                     pkParameterObserved,
                     scaleVectors,
                     labelWrapWidth,
                     vlineIntercept){

  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayNamePKParameter", "displayUnitPKParameter"))
  # Check if scaleVectors is a list
  checkmate::assertList(scaleVectors, names = "named")

  # Check if it contains the expected names
  checkmate::assertNames(names(scaleVectors), must.include = c("simulated", "observed"))

  # Check each entry to ensure they are lists with the correct names
  for (entry in scaleVectors) {
    checkmate::assertList(entry, names = "named")
    checkmate::assertNames(names(entry), must.include = c("color", "fill", "shape"))
  }

  checkmate::assertNumeric(labelWrapWidth,lower = 0, len = 1)
  checkmate::assertNumeric(vlineIntercept,null.ok = TRUE)
}



# support usability --------------------
#' Add Default Configuration for PK Box-and-Whisker Plots
#'
#' Adds default configurations for box-and-whisker plots to the `Plots.xlsx` configuration file.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param sheetName Name of the sheet to create.
#' @param overwrite Logical indicating if existing data should be overwritten.
#' @return NULL (invisible).
#' @export
addDefaultConfigForPKForestPlots <- function(projectConfiguration,
                                             pkParameterDT,
                                             sheetName = "PKParameter_Forest",
                                             overwrite = FALSE) {

  # avoid warnings for global variables during check
  scenarioName <- NULL

  # this function stops in valid runs
  stopHelperFunction()
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  scenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  if (sheetName %in% wb$sheet_names & !overwrite) {
    dtNewHeader <- xlsxReadData(wb, sheetName = sheetName, skipDescriptionRow = TRUE)
    scenarios <- scenarios[!(scenarioName %in% unique(dtNewHeader$scenario))]
  } else {
    dtNewHeader <- data.table(
      level = 1,
      header = "PK-parameter"
    )
  }

  # Create a unique combination of parameters and outputPathId
  dt <- pkParameterDT[, .(pkParameters = paste(unique(parameter), collapse = ', '),
                          outputPathIds = paste(unique(outputPathId), collapse = ', '))]


  # Create a new data.table with all combinations of pkParameters and scenario names
  dtNewConfig <- dt[, .(scenario = scenarios$scenarioName,
                        plotName = 'PKForest',
                        xScale = 'linear,log',
                        facetScale = 'fixed',
                        plot_Absolute_Variance = 1,
                        plot_Absolute_CI = 0,
                        plot_Ratio_Variance = 0,
                        plot_Ratio_CI = 0),
                    by = .(outputPathIds, pkParameters)]


  wb <- addDataAsTemplateToXlsx(
    wb = wb,
    templateSheet = "PKParameter_Forest",
    sheetName = sheetName,
    dtNewData = rbind(dtNewHeader,
                      dtNewConfig, # nolint indentation_linter
                      fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}
