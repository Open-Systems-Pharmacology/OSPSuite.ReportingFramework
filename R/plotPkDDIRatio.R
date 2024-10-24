plotPKRatio <- function(projectConfiguration,
                        subfolder,
                        configTableSheet,
                        pkParameterDT,
                        coefficentOfVariation,
                        nObservationDefault,
                        nBootstrap = 1000,
                        statFun = function(x) exp(mean(log(x))),
                        confLevel = 0.9,
                        seed = 123,
                        vlineIntercept = 1,
                        scaleVactors = list(
                          simulated = list(color ='darkgrey',
                                           fill = 'darkgrey',
                                           shape = 'circle'),
                          observed = list(color = 'darkgrey',
                                          fill = 'lightgrey',
                                          shape = 'triangle filled')),
                        digitsToRound = 2,
                        digitsToShow= 2,
                        xlabel = 'DDI Ratio'){

  checkmate::assertClass(projectConfiguration,classes = 'ProjectConfiguration')
  checkmate::assertString(subfolder)
  checkmate::assertDataTable(pkParameterDT,)
  checkmate::assertNames(names(pkParameterDT),must.include = c("scenarioName", "parameter","individualId", "value" ,"outputPathId", "displayName", "displayUnit"))
  checkmate::assertDouble(coefficentOfVariation,lower = 0,len = 1,finite = TRUE)
  checkmate::assertIntegerish(nObservationDefault,lower = 0,len = 1)
  checkmate::assertIntegerish(nBootstrap,lower = 0,len = 1)
  checkmate::assertIntegerish(seed,len = 1)
  checkmate::assertNumeric(vlineIntercept,any.missing = FALSE,unique = TRUE,null.ok = TRUE)
  checkmate::assertDouble(confLevel,lower = 0,upper = 1,len = 1,finite = TRUE)
  checkmate::assertFunction(statFun)
  checkmate::assertList(scaleVactors,types = 'list')
  checkmate::assertIntegerish(digitsToRound,lower = 0,len = 1)
  checkmate::assertIntegerish(digitsToShow,lower = 0,len = 1)


  # read configuration tables
  configTable <-
    readPKRatioConfigTable(
      sheetName = configTableSheet,
      projectConfiguration = projectConfiguration,
      pkParameterDT = pkParameterDT
    )

  # add displayname of outputPathId
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  pkParameterDT <- merge(data.table::copy(pkParameterDT),
                         dtOutputPaths[,c('outputPathId','displayName')],
                         by = 'outputPathId',
                         suffixes = c('','Output'))

  rmdContainer <- generateRmdContainer(
    projectConfiguration,
    subfolder,
    configTable,
    function(onePlotConfig, rmdContainer, ...) {
      createPKDDIRatioForPlotName(onePlotConfig = onePlotConfig,
                                  rmdContainer = rmdContainer,
                                  pkParameterDT = pkParameterDT,
                                  coefficentOfVariation = coefficentOfVariation,
                                  nObservationDefault = nObservationDefault,
                                  nBootstrap = nBootstrap,
                                  confLevel = confLevel,
                                  statFun = statFun,
                                  seed = seed,
                                  vlineIntercept = vlineIntercept,
                                  scaleVactors = scaleVactors,
                                  digitsToRound = digitsToRound,
                                  digitsToShow = digitsToShow,
                                  xlabel = xlabel)
    }
  )
  return(rmdContainer)

}


readPKRatioConfigTable <- function(projectConfiguration, sheetName,pkParameterDT) {
  # initialize variable used in data.tables
  level <- NULL

  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  configTablePlots <- validateHeaders(configTable)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "displayName"
    ),
    charactersWithMissing =
      c("displayGroup"),
    numericColumns = c(
      "dataMean",
      "dataLowerCI",
      "dataUpperCI",
      "dataNObservation"
    ),
    logicalColumns = NULL,
    numericRangeColumns = NULL,
    subsetList = list(
      scenario = list(
        cols = c("dDIScenario", "controllScenario"),
        allowedValues = unique(pkParameterDT$scenarioName)
      ),
      pkParameter = list(
        cols = c("pkParameter"),
        allowedValues = unique(pkParameterDT$parameter)
      )
    )
  )



  return(configTable)
}


createPKDDIRatioForPlotName <- function(onePlotConfig,
                                        rmdContainer,
                                        pkParameterDT,
                                        coefficentOfVariation,
                                        nObservationDefault,
                                        nBootstrap,
                                        confLevel,
                                        statFun,
                                        seed,
                                        vlineIntercept,
                                        scaleVactors,
                                        digitsToRound,
                                        digitsToShow,
                                        xlabel){
  for (outputPathIdLoop in unique(pkParameterDT$outputPathId)){

    plotData <- prepareDataForDDIRatioPlot(onePlotConfig = onePlotConfig,
                                           pkParameterDT = pkParameterDT[outputPathId == outputPathIdLoop],
                                           coefficentOfVariation = coefficentOfVariation,
                                           nBootstrap = nBootstrap,
                                           nObservationDefault = nObservationDefault,
                                           confLevel = confLevel,
                                           statFun = statFun,
                                           seed = seed)

    plotObject = ospsuite_plotForest(plotData,
                                 vlineIntercept = vlineIntercept,
                                 digitsToRound = digitsToRound,
                                 digitsToShow= digitsToShow,
                                 tableLabels = c('Ratio',
                                                 paste0(confLevel*100,'%\nCI lower'),
                                                 paste0(confLevel*100,'%\nCI upper')),
                                 xlabel = xlabel
    )

    # add color and shape scales
    plotObject <- plotObject +
      scale_color_manual(values = unlist(lapply(scaleVactors,getElement,'color'))) +
      scale_fill_manual(values = unlist(lapply(scaleVactors,getElement,'fill'))) +
      scale_shape_manual(values = unlist(lapply(scaleVactors,getElement,'shape')))


    # export
    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = getCaptionForDDIRatioPlot(outputPathIdLoop = outputPathIdLoop,
                                          plotData = plotData,
                                          pkParameterDT = pkParameterDT
      ),
      figureKey = paste(onePlotConfig$plotName[1],
                        outputPathIdLoop, # nolint indentation_linter
                        sep = "-"
      ),
      width = 30
    )


  }


  return(rmdContainer)

}

# plotHelpers ---------

prepareDataForDDIRatioPlot <- function(onePlotConfig,
                                       pkParameterDT,
                                       coefficentOfVariation,
                                       nBootstrap,
                                       nObservationDefault,
                                       confLevel,
                                       statFun,
                                       seed){


  plotData = data.table()

  for (iRow  in seq_len(nrow(onePlotConfig))){

    configList = onePlotConfig[iRow,]


    nObservations = ifelse(is.na(configList$dataNObservation),
                           nObservationDefault,configList$dataNObservation)

    plotData <-
      rbind(
        plotData,
        rbind(
          cbind(
            configList[,c('displayName', 'displayGroup')],
            calculateRatiosForWorkflow(
              configList = configList,
              pkParameterDT = pkParameterDT[parameter == configList$pkParameter],
              coefficentOfVariation = coefficentOfVariation,
              nObservations = nObservations,
              nBootstrap = nBootstrap,
              confLevel = confLevel,
              statFun = statFun,
              seed = seed
            )
          ) %>%
            dplyr::mutate(type = 'simulated'),
          configList[!is.na(dataMean),c(
                'displayName',
                'displayGroup',
                'dataMean',
                'dataLowerCI',
                'dataUpperCI'
              )] %>%
            data.table::setnames(
              old = c('dataMean', 'dataLowerCI', 'dataUpperCI'),
              new = c('x', 'xmin', 'xmax')
            ) %>%
            dplyr::mutate(type = 'observed')
        ) %>%
          dplyr::mutate(parameter = pkParameterDT[parameter == configList$pkParameter]$displayName[1])
      )

  }

  if (any(is.na(plotData$x))){
    warning(paste('PKParameter for DDI Ratio contains NAs, rows will be omitted'))
    plotData <- plotData[!is.na(x)]
  }

  # make sure of order by creating factors
  plotData$displayName = factor(plotData$displayName,
                                levels = unique(configTable$displayName),
                                ordered = TRUE)

  plotData$type = factor(plotData$type,
                         levels = unique(plotData$type),ordered = TRUE)

  plotData$displayGroup = factor(plotData$displayGroup,
                                 levels = unique(plotData$displayGroup),ordered = TRUE)


  return(plotData)


}

calculateRatiosForWorkflow = function(configList,
                                      pkParameterDT,
                                      coefficentOfVariation,
                                      nObservations,
                                      nBootstrap,
                                      confLevel,
                                      statFun = statFun,
                                      seed){
  # add inter-occasional variability
  set.seed(seed)

  sigma <- sqrt(log(coefficentOfVariation^2+1))
  pkParameterDT[,valueIOV := value * exp(rnorm(n = .N, mean = 0, sd = sigma))]

  # calculate ratio
  dt <- merge(pkParameterDT[scenarioName == configList$controllScenario],
        pkParameterDT[scenarioName == configList$dDIScenario],
        by = 'individualId',
        all = TRUE,
        suffixes = c('.control','.DDI'))%>%
    . [,ratio := valueIOV.DDI/valueIOV.control]

  # adjust maximal number of observations
  if (nrow(dt) <= nObservations*1.1) {
    nObservations = floor(nrow(dt)*0.9)
    warning(message = paste('reduce nObservations as simulated data set is to small:',
                            'nObservations =',nObservations,
                            'n simulated ratios', nrow(dt) ))
  }

  # start boostrapping
  set.seed(seed)


  getStatSample <- function(y, statFun) {
    return(statFun(sample(x = y, size = nObservations, replace = FALSE)))
  }

  myBootstrap <- function(y, statFun) {
    y <- y[!is.na(y) & y > 0]
    bstrap <- do.call(rbind, replicate(nBootstrap, getStatSample(y, statFun), simplify = FALSE))

    return(stats::setNames(c(quantile(bstrap, probs = c((1 - confLevel) / 2, 0.5, 1 - ((1 - confLevel) / 2)))),
                             c('xmin', 'x', 'xmax')))
  }

  dtBootstrapped = dt[,as.list(myBootstrap(ratio,statFun))]


  return(dtBootstrapped)
}

getCaptionForDDIRatioPlot<- function(outputPathIdLoop,
                                     plotData,
                                     pkParameterDT
){

  paste0('Simulated ',
        ifelse('observed' %in% unique(plotData$type),' and observed ',''),
    concatWithAnd(paste(unique(pkParameterDT[outputPathId == outputPathIdLoop,]$displayName),'ratios')),
    ' of ',pkParameterDT[outputPathId == outputPathIdLoop,]$displayNameOutput[1])

}
