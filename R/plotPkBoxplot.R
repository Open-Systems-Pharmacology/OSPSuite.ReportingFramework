#' Plot PK Box-and-Whisker
#'
#' Generates box-and-whisker plots for pharmacokinetic (PK) parameters based on
#' a provided project configuration and plot configuration. This function creates
#' visual representations of the distribution of PK parameters across different
#' scenarios, allowing for comparison of absolute values and ratios.
#'
#' The function supports customization options such as specifying the angle of the
#' x-axis text, color schemes, and facet aspect ratios.
#'
#' @param projectConfiguration A ProjectConfiguration object that contains
#'   settings and paths relevant to the project.
#' @param onePlotConfig A data.table containing configuration settings for a single plot,
#'   including details about which plots to generate and their formatting.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param percentiles A numeric vector specifying the percentiles to calculate
#'   for the box-and-whisker plots. Default is retrieved from project options.
#' @param xAxisTextAngle An integer specifying the angle (in degrees) for
#'   rotating the x-axis text labels. Default is 0 (no rotation).
#' @param colorVector A named vector specifying colors for the plots. Names should correspond to the characters
#'  defined in the configtable column colorLegend. If your color legend is for example 'DDI|control',
#'  your color vector could be colorVector = c(DDI = 'red',control = 'blue'), if no color is defined default values are used.
#' @param facetAspectRatio A numeric value specifying the aspect ratio for
#'   faceting the plots. Default is 0.5, which may need adjustment based on the
#'   number of facets and plot dimensions.
#' @param nBootstrap An integer specifying the number of bootstrap samples to
#'   use when calculating ratios. Default is 1000.
#' @param ... Additional arguments passed to plotting functions for further
#'   customization.
#' @return A list of generated plots. Each plot is an object that can be rendered
#'   using ggplot2 or similar plotting systems. The list may include both absolute
#'   and ratio plots depending on the configuration.
#' @details
#' When calculating ratios, the function distinguishes between two cases:
#'
#' - **Case 1**: If the scenarios being compared are based on the **same populations**,
#'   (same `PopulationId` in scenario configuration) the plots will show summary statistics of individual ratios.
#'
#' - **Case 2**: If the scenarios are based on **different populations**, the plots
#'   will display the ratio of summary statistics, using either bootstrapping methods
#'   or analytical solutions for "geo mean","geo standard deviation","geo CV".
#'
#'
#' @examples
#' # Example usage of plotPKBoxwhisker
#' \dontrun{
#' plotList <- plotPKBoxwhisker(projectConfiguration = myProjectConfig,
#'                                onePlotConfig = myPlotConfig,
#'                                pkParameterDT = myPKData,
#'                                percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
#'                                xAxisTextAngle = 45,
#'                                colorVector = c(DDI = "red", control = "blue"),
#'                                facetAspectRatio = 1,
#'                                nBootstrap = 1000)
#' }
#' @export
plotPKBoxwhisker <- function(projectConfiguration,
                             onePlotConfig,
                             pkParameterDT,
                             percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles),
                             xAxisTextAngle = 0,
                             colorVector = c(scenario = NA, referenceScenario = NA),
                             facetAspectRatio = 0.5,
                             nBootstrap = 1000,
                             ...){

  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayNamePKParameter", "displayUnitPKParameter"))
  checkmate::assertNumeric(xAxisTextAngle,any.missing = FALSE)
  checkmate::assertNumeric(facetAspectRatio,any.missing = FALSE)

  colorVector = getColorVectorForLegend(colorVector  = colorVector,
                                        colorLegend = onePlotConfig[['colorLegend']][1])

  plotList <- list()
  for (plotType in c("Absolute", "Ratio")) {
    if (as.logical(onePlotConfig[[paste0('plot_',plotType)]][1])){
      plotList <- c(plotList,
                    generateBoxwhiskerPlotForPlotType(onePlotConfig = onePlotConfig,
                                                      pkParameterDT = pkParameterDT,
                                                      percentiles = percentiles,
                                                      xAxisTextAngle =xAxisTextAngle,
                                                      colorVector = colorVector,
                                                      facetAspectRatio = facetAspectRatio,
                                                      nBootstrap = nBootstrap,
                                                      asRatio = plotType == 'Ratio',
                                                      ...)
      )
    }
  }
  return(plotList)
}
#' Generate Box-and-Whisker Plot for a Specific Plot Type
#'
#' Creates box-and-whisker plots for either absolute values or ratios of PK parameters.
#'
#' @param onePlotConfig Configuration for a single plot.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param percentiles A vector of percentiles to calculate.
#' @param xAxisTextAngle Angle for x-axis text rotation.
#' @param colorVector A named vector for colors.
#' @param facetAspectRatio Aspect ratio for facets.
#' @param nBootstrap Number of bootstrap samples.
#' @param asRatio Logical indicating if the plot is for ratios.
#' @param ... Additional arguments passed to plotting functions.
#' @return A list of generated plots for the specified plot type.
#' @keywords internal
generateBoxwhiskerPlotForPlotType <- function(onePlotConfig,
                                              pkParameterDT,
                                              percentiles,
                                              xAxisTextAngle,
                                              colorVector,
                                              facetAspectRatio,
                                              nBootstrap,
                                              asRatio,
                                              ...) {

  # Determine ratio mode
  ratioMode <- getRatioMode(onePlotConfig, pkParameterDT, asRatio)
  # Prepare data for plotting
  plotData <- prepareDataForPKBoxplot(onePlotConfig = onePlotConfig,
                                      pkParameterDT = pkParameterDT,
                                      colorVector = colorVector,
                                      ratioMode = ratioMode,
                                      nBootstrap = nBootstrap,
                                      percentiles = percentiles)
  if (nrow(plotData) == 0) {
    warning(paste('No data for',onePlotConfig$plotName[1]))
    return(list())
  }

  # Determine facet columns
  nFacetColumns <- NULL
  if(dplyr::n_distinct(plotData$plotTag) > 1) nFacetColumns =  1

  # Initialize plot list
  plotList <- list()

  # Loop through unique plot names
  for (plotNameLoop in unique(plotData$plotName)) {
    plotDataPk <- plotData[plotName == plotNameLoop]

    # Loop through yScale values
    for (yScale in splitInputs(onePlotConfig$yScale[1])) {
      plotObject <- createBaseBoxWhisker(plotDataPk = plotDataPk,
                                         yScale = yScale,
                                         ratioMode = ratioMode,
                                         colorVector = colorVector,
                                         onePlotConfig = onePlotConfig,
                                         ...)

      # Add facets
      plotObject <- addFacets(plotObject = plotObject,
                              facetScale = onePlotConfig$facetScale[1],
                              facetAspectRatio = facetAspectRatio,
                              nFacetColumns = nFacetColumns)

      # Adjust x-axis text angle
      if (xAxisTextAngle > 0) {
        plotObject <- plotObject +
          theme(axis.text.x = element_text(angle = xAxisTextAngle, hjust = 1))
      }

      # Prepare for export
      plotObject <- setExportAttributes(
        object = plotObject,
        caption = getCaptionForBoxwhiskerPlot(plotDataPk = plotDataPk,
                                              yScale = yScale,
                                              percentiles = percentiles,
                                              plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
                                              ratioMode = ratioMode))
      # Create figure key and store plot
      figureKey <- paste(plotNameLoop,
                         ifelse(yScale == "log", "log", "linear"),
                         ifelse(asRatio, 'ratio', 'abs'),
                         sep = "-")
      plotList[[figureKey]] <- plotObject
    }

    # Generate table for the last plot
    exportList = list()

    for(plotDataPkTag in split(plotDataPk,by = 'plotTag')){

      tableKey <- paste(plotNameLoop, ifelse(asRatio, 'ratio', 'abs'), sep = "-")
      if (uniqueN(plotDataPk$plotTag) > 1) tableKey <- paste(tableKey,plotDataPkTag$plotTag[1],sep = "-")

      plotList[[tableKey]] <- addAsTable(plotDataPk = plotDataPkTag,
                                         plotObject = plotObject,
                                         onePlotConfig = onePlotConfig,
                                         ratioMode = ratioMode,
                                         percentiles = percentiles)
    }
  }
  return(plotList)
}

#' Prepare Data for PK Boxplot
#'
#' Prepares and cleans data for box-and-whisker plotting.
#'
#' @param onePlotConfig Configuration for a single plot.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param colorVector A named vector for colors.
#' @param ratioMode Mode indicating if the plot is for ratios.
#' @param nBootstrap Number of bootstrap samples.
#' @param percentiles A vector of percentiles to calculate.
#' @return A data.table prepared for plotting.
#' @keywords internal
prepareDataForPKBoxplot <- function(onePlotConfig,pkParameterDT,colorVector,ratioMode,nBootstrap,percentiles){

  plotData <- data.table::copy(onePlotConfig)

  plotData <- separateAndTrim(plotData, "outputPathIds")
  plotData <- separateAndTrim(plotData, "pkParameters")

  plotData <- plotData %>%
    dplyr::select(!c('level', 'header')) %>%
    merge(pkParameterDT %>%
            unique() %>%
            data.table::setnames(old = c('parameter', 'scenarioName'),
                                 new = c('pkParameter','scenario')),
          by = c('scenario', 'pkParameter', 'outputPathId')
    ) %>%  merge(configEnv$outputPaths[, c('outputPathId', 'displayNameOutputs')],
                 by = 'outputPathId')


  if (nrow(plotData) > 0){

    if (dplyr::n_distinct(plotData$pkParameter) > 1){
      plotData[,plotName := paste(plotName,pkParameter,sep = '_')]
    }

    if (ratioMode != 'none'){
      plotData <- plotData[!is.na(referenceScenario)]

      if (ratioMode == 'individualRatios'){
        plotData <- merge(plotData,
                          pkParameterDT[,c('scenarioName',"parameter","individualId","outputPathId","value","populationId")] %>%
                            setnames(old = c('scenarioName','parameter'),
                                     new = c('referenceScenario','pkParameter')),
                          by = c('referenceScenario',"pkParameter","individualId","outputPathId"),
                          suffixes = c('','.reference'))
        plotData[,ratio := value/value.reference]
      } else  if (ratioMode == 'ratioOfPopulation'){
        plotData <-  merge(plotData[, !c("individualId", "value"), with = FALSE] %>%  unique(),
                           getPopulationRatioByBootstrapping(plotData = plotData,
                                                             pkParameterDT = pkParameterDT,
                                                             nBootstrap = nBootstrap,
                                                             percentiles = percentiles),
                           by = c('scenario',"pkParameter","outputPathId"))
      } else {
        stop(paste('RatioMode is unkown:',ratioMode))
      }
    } else {
      plotData[,isReference := scenario %in% referenceScenario, by = c('plotName')]
      plotData[,colorIndex := ifelse(isReference == TRUE,names(colorVector)[2],names(colorVector)[1])]
      plotData$colorIndex <- factor(plotData$colorIndex,levels = names(colorVector))
    }

    # Ensure order by creating factors
    plotData$displayNameOutput <- factor(plotData$displayNameOutput,
                                         levels = unique(plotData$displayNameOutput),
                                         ordered = TRUE)

    plotData$scenarioShortName <- factor(plotData$scenarioShortName,
                                         levels = unique(onePlotConfig$scenarioShortName),
                                         ordered = TRUE)
    # add Tag for faceting
    plotData[,plotTag := generatePlotTag(as.numeric(displayNameOutput))]
  }

  return(plotData)
}
#' Add Summary Table from Plot Data
#'
#' Generates a summary table from the box-and-whisker plot data.
#'
#' @param plotDataPk A data.table containing plot data.
#' @param plotObject The plot object to extract statistics from.
#' @param ratioMode Mode indicating if the plot is for ratios.
#' @param onePlotConfig Configuration for a single plot.
#' @param percentiles A vector of percentiles to calculate.
#' @return A data.table summarizing the plot data.
#' @keywords internal
addAsTable <- function(plotDataPk,plotObject,ratioMode,onePlotConfig,percentiles){

  dtExport <- switch (
    ratioMode,
    'none' = plotDataPk %>%
      data.table::setDT() %>%
      .[, as.list(plotObject$statFun(value)),
        by = intersect(c("scenarioShortName","colorIndex"),names(plotDataPk))
      ],
    'individualRatios' = plotDataPk %>%
      data.table::setDT() %>%
      .[, as.list(plotObject$statFun(ratio)),
        by = intersect(c("scenarioShortName","colorIndex"),names(plotDataPk))
      ],
    'ratioOfPopulation' = plotDataPk[,c('scenarioShortName',
                                        'N','N reference',
                                        'arithMean', 'arithSd', 'arithCV',
                                        'geoMean','geoSd','geoCV',
                                        'ymin','lower','middle','upper','ymax')] %>%
      setnames(old = c('ymin','lower','middle','upper','ymax',
                       'arithMean','arithSd','arithCV',
                       'geoMean','geoSd','geoCV'),
               new = c(paste(scales::label_ordinal()(x = percentiles * 100),'percentile'),
                       "arith mean","arith standard deviation","arith CV",
                       "geo mean","geo standard deviation","geo CV"))
  )
  # reorder
  setorderv(dtExport,'scenarioShortName')

  if ('colorIndex' %in% names(dtExport)){
    dtExport[,scenario := paste(scenarioShortName,colorIndex)]
    dtExport[,scenarioShortName := NULL]
    dtExport[,colorIndex := NULL]
    dtExport$scenario <- factor(dtExport$scenario,
                                levels = unique(dtExport$scenario),
                                ordered = TRUE)
  } else {
    setnames(dtExport, old = 'scenarioShortName', new = 'scenario')
    dtExport$scenario <- factor(dtExport$scenario,
                                levels = levels(plotDataPk$scenarioShortName),
                                ordered = TRUE)
  }
  setcolorder(dtExport,c('scenario'))

  dtExport <- setExportAttributes(
    object = dtExport,
    caption = getCaptionForBoxwhiskerPlot(plotDataPk = plotDataPk,
                                          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
                                          isPlotCaption = FALSE,
                                          ratioMode = ratioMode)
  )

  return(dtExport)
}

# auxiliaries  ------------
#' Calculate Population Ratio by Bootstrapping
#'
#' Calculates population ratios using bootstrapping techniques.
#'
#' @param plotData A data.table containing plot data.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param nBootstrap Number of bootstrap samples.
#' @param percentiles A vector of percentiles to calculate.
#' @return A data.table containing bootstrapped ratio statistics.
#' @keywords internal
getPopulationRatioByBootstrapping <- function(plotData,pkParameterDT,nBootstrap,percentiles){

  # Apply the function to each unique combination of PKParameter and outputPathId
  resultList <- lapply(split(plotData,by = c('outputPathId','pkParameter','scenario')), function(dtDefault) {
    dtReference <- merge(dtDefault %>%
                           dplyr::select('outputPathId','pkParameter','referenceScenario') %>%
                           unique() %>%
                           setnames(old = 'referenceScenario', new = 'scenario'),
                         pkParameterDT[,c('scenarioName',"parameter","individualId","outputPathId","value","populationId")] %>%
                           setnames(old = c('scenarioName','parameter'),
                                    new = c('scenario','pkParameter')),
                         by = c('scenario',"pkParameter","outputPathId"))

    setSeedForPKParameter(scenarioName = dtDefault$scenario[1],referenceScenarion = dtReference$scenario[1],
                          outputPathId = dtDefault$outputPathId[1],pkParameter = dtDefault$pkParameter[1])


    boxWhiskerAggregationFun = function(y){
      y <- y[(!is.na(y))]
      arithMean <- mean(y)
      arithSd <- sd(y)
      arithCV <- arithSd/arithMean
      rQuantiles <- stats::setNames(quantile(y, probs = percentiles,names = FALSE),
                                    c('ymin','lower','middle','upper','ymax'))
      return(as.list(c(arithMean = arithMean,
                       arithSd = arithSd,
                       arithCV = arithCV,
                       rQuantiles))
      )


    }

    result <- aggregateRatiosByBootstrapping(nBootstrap = nBootstrap,
                                             value =  dtDefault[!is.na(value)]$value,
                                             valueReference = dtReference[!is.na(value)]$value,
                                             aggregationFun = boxWhiskerAggregationFun,
                                             confLevel = NULL)

    # add quantities which are calculated analytical
    lValues = log( dtDefault[!is.na(value) & value>0]$value)
    lReferenceValues = log( dtReference[!is.na(value) & value>0]$value)
    result[['geoMean']] <- exp(mean(lValues) - mean(lReferenceValues))
    result[['geoSd']] <- exp(sqrt((sd(lValues))^2 + (sd(lReferenceValues))^2))
    result[['geoCV']] <- sqrt(exp((sd(lValues))^2 + (sd(lReferenceValues))^2) - 1)
    # add identifier and counts
    result[['scenario']] <- dtDefault$scenario[1]
    result[['outputPathId']] <- dtDefault$outputPathId[1]
    result[['pkParameter']] <- dtDefault$pkParameter[1]
    result[['N']] <- length(dtDefault[!is.na(value)]$value)
    result[['N reference']] <- length(dtReference[!is.na(value)]$value)
    return(result)

  })

  # Combine results into a single data.table
  return(rbindlist(resultList))

}
#' Get Caption for Box-and-Whisker Plot
#'
#' Generates a caption for the box-and-whisker plot based on the data and configuration.
#'
#' @param plotDataPk A data.table containing plot data.
#' @param percentiles A vector of percentiles to calculate.
#' @param yScale Scale type (linear or log).
#' @param plotCaptionAddon Additional text for the caption.
#' @param isPlotCaption Logical indicating if the caption is for the plot.
#' @param ratioMode Mode indicating if the plot is for ratios.
#' @return A character string containing the caption.
#' @keywords interal
getCaptionForBoxwhiskerPlot <-function(plotDataPk,
                                       percentiles = NULL,
                                       yScale = NULL,
                                       plotCaptionAddon,
                                       isPlotCaption = TRUE,
                                       ratioMode) {
  dtCaption <-
    plotDataPk[, c(
      "scenarioLongName",
      'displayNameOutput',
      'plotTag',
      "plotCaptionAddon",
      "displayNamePKParameter",
      "displayNameOutput"
    )] %>%  unique()


  captiontext <- switch(ratioMode,
                        'none' = paste("Population summary statistics of",
                                       dtCaption$displayNamePKParameter[1]),
                        'individualRatios' = paste('Population summary statistics of',
                                                   dtCaption$displayNamePKParameter[1], 'ratios'),
                        'ratioOfPopulation' = paste('Ratio of population summary statistics of',
                                                    dtCaption$displayNamePKParameter[1]))

  captiontext <- paste(captiontext,
                       "of",
                       pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"))

  captiontext <- addCaptionTextAddon(captiontext,plotCaptionAddon)

  if (isPlotCaption){
    captiontext <- paste(captiontext,
                         "Shown as box-whisker plot, which indicates the",
                         paste(formatPercentiles(percentiles,suffix = '',allAsPercentiles = TRUE),collapse = ', '),'percentiles',
                         "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
                         "y-scale")
  }
  return(captiontext)
}

#' Create a Box and Whisker Plot Object
#'
#' This helper function generates a box and whisker plot object based on the provided
#' plot data and configuration. It supports different ratio modes and applies the
#' appropriate aesthetics and scales to the plot.
#'
#' @param plotDataPk A data frame containing the plot data for a specific PK parameter.
#' @param yScale A character string indicating the scale for the y-axis (e.g., "linear", "log").
#' @param ratioMode A character string indicating the mode of ratio to be used for plotting
#'                  (e.g., "individualRatios", "ratioOfPopulation", "none").
#' @param colorVector A vector of colors to be used for filling the plot.
#' @param onePlotConfig A list containing configuration settings for the plot.
#' @param ... Additional arguments to be passed to the plotting functions.
#'
#' @return A ggplot object representing the box and whisker plot.
#' @keywords internal
createBaseBoxWhisker <- function(plotDataPk, yScale, ratioMode, colorVector, onePlotConfig, ...) {
  ylabelUnit <- 'ratio'
  switch(ratioMode,
         'individualRatios' = {
           plotObject <- ospsuite.plots::plotBoxWhisker(data = plotDataPk,
                                                        mapping = aes(x = scenarioShortName, y = ratio),
                                                        yscale = yScale,
                                                        yscale.args = getXorYlimits(onePlotConfig, yScale, ...))
         },
         'ratioOfPopulation' = {
           plotObject <- ospsuite.plots::initializePlot()  +
             geom_boxplot(data = plotDataPk,
                          mapping = aes(x = scenarioShortName, lower = lower, upper = upper,
                                        middle = middle, ymin = ymin, ymax = ymax),
                          stat = "identity")

           plotObject <- plotObject %>%
             ospsuite.plots::addYscale(plotObject = .,
                                       yscale = yScale,
                                       yscale.args = getXorYlimits(onePlotConfig, yScale, ...))
         },
         'none' = {
           ylabelUnit <- ifelse(plotDataPk$displayUnit[1] != '',
                                paste0('[', plotDataPk$displayUnit[1], ']'), '')

           plotObject <- ospsuite.plots::plotBoxWhisker(data = plotDataPk,
                                                        mapping = aes(x = scenarioShortName, y = value, fill = colorIndex),
                                                        yscale = yScale,
                                                        yscale.args = getXorYlimits(onePlotConfig, yScale, ...)) +
             scale_fill_manual(values = colorVector) +
             theme(legend.title = element_blank())
         }
  )


  plotObject <- plotObject +
    labs(x = '', y = paste(plotDataPk$displayNamePKParameter[1], ylabelUnit))

  return(plotObject)
}
#' Get Ratio Mode for Plot Configuration
#'
#' This function determines the ratio mode for a given plot configuration based on the provided parameters.
#' It checks if the scenarios in the plot configuration have the same or different base populations.
#'
#' @param onePlotConfig A data frame containing the plot configuration with columns 'plotName', 'scenario', and 'referenceScenario'.
#' @param pkParameterDT A data frame containing parameter details, including 'scenarioName' and 'populationId'.
#' @param asRatio A logical value indicating whether to calculate the ratio mode. If FALSE, the function returns 'none'.
#'
#' @return A character string indicating the ratio mode. Possible values are:
#' - 'none' if `asRatio` is FALSE.
#' - 'individualRatios' if all population IDs match between scenarios.
#' - 'ratioOfPopulation' if all population IDs are different between scenarios.
#'
#' @keywords internal
getRatioMode <- function(onePlotConfig, pkParameterDT, asRatio) {
  if (!asRatio) return('none')

  pkParameterDTScenarios <- pkParameterDT[, c('scenarioName', 'populationId')] %>%
    unique()

  dtPop <- merge(onePlotConfig[, c('plotName', 'scenario', 'referenceScenario')] %>%
                   unique(),
                 pkParameterDTScenarios,
                 by.x = 'scenario',
                 by.y = 'scenarioName') %>%
    merge(pkParameterDTScenarios,
          by.x = 'referenceScenario',
          by.y = 'scenarioName',
          suffixes = c('', '.reference'))

  if (all(dtPop$populationId == dtPop$populationId.reference)) {
    ratioMode <- 'individualRatios'
  } else if (all(dtPop$populationId != dtPop$populationId.reference)) {
    ratioMode <- 'ratioOfPopulation'
  } else {
    print(dtPop)
    stop('Within one plot you must either compare always scenarios with the same base population or
             always scenarios with different base populations')
  }

  return(ratioMode)
}

# validation ------------
#' Validate PK Box-and-Whisker Configuration Table
#'
#' Validates the configuration table for PK box-and-whisker plots.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#'
#' @keywords internal
validatePKBoxwhiskerConfig <- function(configTable, pkParameterDT,...) {

  configTablePlots <- validateHeaders(configTable)
  validateOutputIdsForPlot()
  validateDataGroupIdsForPlot()

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("plotName","scenario","scenarioShortName",'pkParameters','outputPathIds'),
    charactersWithMissing = c("plotCaptionAddon","colorLegend"),
    logicalColumns = c("plot_Absolute","plot_Ratio"),
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
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
      ),
      yscale = list(
        cols = c("yscale"),
        allowedValues = c("linear", "log")
      ),
      facetScale = list(
        cols = c("facetScale"),
        allowedValues = c("fixed", "free", "free_x", "free_y")
      )
    )
  )

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      'pkParameters',
      'colorLegend',
      "yScale",
      "plotCaptionAddon",
      "ylimit_linear",
      "ylimit_log",
      "facetScale",
      "plot_Absolute",
      "plot_Ratio"

    ))

  tmp <- separateAndTrim(configTablePlots, "outputPathIds")
  tmp <- separateAndTrim(tmp, "pkParameters")
  tmp <- tmp[,c('plotName','scenario','outputPathId','pkParameter')]
  if (any(duplicated(tmp))){
    tmp <- duplicated(tmp)
    stop(paste('Per plot only one combination of scenario, outputPathId and pkParameter is allowed. Please check plot',
               paste(tmp$plotName %>%  unique(),collapse = ', ')))
  }

  validateColorLegend(dt = configTablePlots[!is.na(referenceScenario)])


  # check if reference Scenarios are there
  validateExistenceOfReferenceForRatio(configTablePlots = configTablePlots[plot_Ratio == TRUE ])

  return(invisible())
}

validateExistenceOfReferenceForRatio <- function(configTablePlots){

  if (nrow(configTablePlots) == 0)   return(invisible())

  # check if reference Scenarios are there
  tmp <- configTablePlots[,.(isValid = any(!is.na(referenceScenario))),by = plotName]
  if (any(tmp$isValid == FALSE)){
    stop(paste('For ratio plots at lease one reference scenario has to be selected. Check PlotName',paste(tmp$plotName,collapse = ', ')))
  }
  return(invisible())

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
addDefaultConfigForPKBoxwhsikerPlots <- function(projectConfiguration,
                                                 pkParameterDT,
                                                 sheetName = "PKParameter_Boxplot",
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
  dt <- pkParameterDT[, .(pkParameters = paste(unique(parameter), collapse = ', ')), by = outputPathId] %>%
    .[, .(outputPathIds = paste(unique(outputPathId), collapse = ', ')), by = pkParameters]

  # Create a new data.table with all combinations of pkParameters and scenario names
  dtNewConfig <- dt[, .(scenario = scenarios$scenarioName,
                        yScale = 'linear, log',
                        facetScale = 'fixed',
                        plot_Absolute = 1,
                        plot_Ratio = 1),
                    by = .(outputPathIds, pkParameters)]


  wb <- addDataAsTemplateToXlsx(
    wb = wb,
    templateSheet = "PKParameter_Boxplot",
    sheetName = sheetName,
    dtNewData = rbind(dtNewHeader,
                      dtNewConfig, # nolint indentation_linter
                      fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}
