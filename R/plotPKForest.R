#' @title PK Forest Plots
#'
#' @description
#' This is one of a group of functions which generates PK forest plots for pharmacokinetic (PK) parameters, providing visualizations
#' that can include absolute values or ratios, along with options for displaying variance
#' or confidence intervals. These functions allow for the comparison of simulated and observed
#' PK parameter data across different scenarios, aiding in the interpretation of variability
#' and uncertainty in the estimates.
#'
#' Available functions are:
#'
#' \describe{
#'   \item{\code{plotPKForestAbsoluteValuesWithVariance}}{Generates a PK forest plot displaying absolute values along with variance.}
#'   \item{\code{plotPKForestAbsoluteValuesWithCI}}{Generates a PK forest plot displaying absolute values along with confidence intervals using bootstrapping.}
#'   \item{\code{plotPKForestRatiosWithVariance}}{Generates a PK forest plot displaying ratios of PK parameters along with variance.}
#'   \item{\code{plotPKForestRatiosWithCI}}{Generates a PK forest plot displaying ratios of PK parameters along with confidence intervals using bootstrapping.}
#' }
#'
#'
#' @details
#'
#' For the ratio calculation, the function distinguishes between two cases:
#'
#'
#'\describe{
#'   \item{Case 1:}{If the scenarios being compared are based on the \strong{same populations}
#'   (same `PopulationId` in scenario configuration), the plots will show summary statistics of
#'   individual ratios.}
#'   \item{Case 2:}{If the scenarios are based on \strong{different populations}, the plots
#'   will display the ratio of summary statistics, using bootstrapping methods. During each bootstrapping
#'   step, `nSamples` are drawn and aggregated. `nSamples` is the minimum number of individuals from both populations.
#'   Displayed are the median values of the aggregated results.}
#' }
#'
#' To support the creation of the configuration table a support function \code{addDefaultConfigForPKForestPlots} is available
#'
#' For the bootstrapping process, a unique seed is set for each combination of scenario, referenceScenario (if available),
#' outputPathId, and PKParameter identifier. This ensures that results are consistent when a specific
#' combination is used in different plots and that they are reproducible.
#'
#' @param projectConfiguration A ProjectConfiguration object containing the
#'   necessary settings and file paths for the project.
#' @param onePlotConfig A data.table containing configuration settings for a
#'   single plot, including plot name, x-axis scale, and other aesthetic settings.
#' @param pkParameterDT A data.table containing simulated PK parameter data,
#'   including columns for scenario names, parameters, values, and output paths.
#' @param pkParameterObserved Optional data.table containing observed PK parameter
#'   data for comparison, which can include columns for observed values and associated
#'   metadata.
#' @param aggregationFlag A character string indicating the method of aggregation
#'   for variance calculation. Options include "GeometricStdDev", "ArithmeticStdDev",
#'   and "Percentiles".
#'  (for `plotPKForestAbsoluteValuesWithVariance` and `plotPKForestRatiosWithVariance`):
#' @param percentiles A numeric vector specifying which percentiles to calculate
#'   and display in the plot, only relevant if `aggregationFlag = "Percentiles"`
#'   (for `plotPKForestAbsoluteValuesWithVariance` and `plotPKForestRatiosWithVariance`):
#' @param statFun A named list of functions used for statistical aggregation, typically
#'   including the geometric mean function for bootstrapping.
#'   This must be a function accepting as input a numeric vector and returning a numeric value.
#'   The input is formatted as a named list, the name is used in plot legends and captions
#'    c('geometric mean' = function(y) exp(mean(log(y[y>0]))))
#'  (for `plotPKForestAbsoluteValuesWithCI` and `plotPKForestRatiosWithCI`)
#' @param confLevel A numeric value between 0 and 1 indicating the desired confidence
#'   level for the intervals (e.g., 0.9 for 90% confidence).
#'  (for `plotPKForestAbsoluteValuesWithCI` and `plotPKForestRatiosWithCI`)
#' @param coefficientOfVariation A numeric value representing the coefficient of
#'   variation, which may be used to assess variability in the data. Only relevant for crossover studies (see details)
#'  (for `plotPKForestAbsoluteValuesWithCI` and `plotPKForestRatiosWithCI`)
#' @param nObservationDefault An integer specifying the default number of observations
#'   to use for bootstrapping.
#'   (for `plotPKForestAbsoluteValuesWithCI` and `plotPKForestRatiosWithCI`)
#' @param nBootstrap An integer indicating the number of bootstrap samples to draw
#'   for calculating confidence intervals.
#'   (for `plotPKForestAbsoluteValuesWithCI`, `plotPKForestRatiosWithCI` and
#'   `plotPKForestRatiosWithVariance`)
#' @param scaleVectors A list containing user-defined scale vectors for the plot aesthetics
#'   for simulated and observed data. The list can have up to two components: `simulated` and
#'   `observed`, each containing a list of properties (`color`, `fill`, `shape`) to modify the
#'   default scale vector settings.
#' @param labelWrapWidth Numeric value specifying the maximum width for wrapping
#'   labels in the plot to enhance readability.
#' @param vlineIntercept Optional numeric value indicating where to draw a vertical
#'   line on the plot, often used to denote a reference value.
#' @param withTable logical, if TRUE (default) values are displaye as table beside the plot
#' @param relWidths Optional numeric vector specifying relative widths for the plot and table.
#' @param digitsToRound An integer specifying the number of digits to round in the
#'   displayed values.
#' @param digitsToShow An integer specifying the number of digits to show in the
#'   displayed values.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param ratioMode Mode for ratio calculations.
#'
#' @return A list containing the generated plot objects.
#'
#' @seealso
#' \code{\link{plotPKForestAbsoluteValuesWithVariance}}, \code{\link{plotPKForestAbsoluteValuesWithCI}},
#' \code{\link{plotPKForestRatiosWithVariance}}, \code{\link{plotPKForestRatiosWithCI}},
#' \code{\link{addDefaultConfigForPKForestPlots}}
#'
#' @keywords internal
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
                         confLevel = NULL,
                         vlineIntercept,
                         withTable = TRUE,
                         relWidths = NULL,
                         digitsToRound = 2,
                         digitsToShow = 2,
                         scaleVectors,
                         labelWrapWidth = 2) {
  # separates the comma separate inputs to rows
  onePlotConfig <- separateAndTrim(data = onePlotConfig,columnName = 'outputPathIds') %>%
    separateAndTrim(columnName = 'pkParameters')
  # use empty string for grouping
  onePlotConfig[is.na(scenarioGroup),scenarioGroup := '']
  # scenarios without referenceScenario  are ignored in ratio mode
  if (ratioMode != 'none') onePlotConfig <- onePlotConfig[!is.na(referenceScenario)]

  scaleVectors <- updateScalevector(scaleVectorsInput = scaleVectors)

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

  plotDataGroup <- prepareDataForPKForest(
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    ratioMode = ratioMode,
    asCI = asCI,
    aggregationFun = aggregationFun,
    aggregationFlag = aggregationFlag,
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    nObservationDefault
  )

  plotList <- list()

  for (groupName in names(plotDataGroup)){

    columnList <-
      getColumnSelectionForPKForest(plotData = plotDataGroup[[groupName]], ratioMode = ratioMode)

    tableLabels <-
      getTableLabelsForPKForest(
        plotData = plotDataGroup[[groupName]],
        percentiles = percentiles,
        aggregationFun = aggregationFun,
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
                                               direction = 'x'),
                   tableColumns = names(tableLabels),
                   tableLabels = tableLabels,
                   labelWrapWidth = labelWrapWidth,
                   digitsToRound = digitsToRound,
                   digitsToShow = digitsToShow,
                   withTable = withTable)

      combinedObject$plotObject <-
        adjustPkForestPlotObject(plotObject = combinedObject$plotObject,
                                 scaleVectors = scaleVectors,
                                 vlineIntercept = vlineIntercept)

      if (!is.null(relWidths)) {
        combinedObject$relWidths = relWidths
      } else if (uniqueN(plotDataGroup[[groupName]]$dataType) > 1){
        combinedObject$relWidths = c(5,2)
      }


      # Prepare for export
      combinedObject <- setExportAttributes(
        object = combinedObject,
        caption = getCaptionForForestPlot(plotData = plotDataGroup[[groupName]],
                                          xScale = xScale,
                                          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
                                          ratioMode = ratioMode,
                                          asCI = asCI),
        footNoteLines =
          getFootnoteLinesForForrestPlots(
            plotData = plotDataGroup[[groupName]],
            ratioMode = ratioMode,
            asCI = asCI,
            dtDataReference = NULL
          ),
        exportArguments = list(width = 22,
                               heightToWidth =
                                 plotDataGroup[[groupName]][,uniqueN(.SD),
                                                            .SDcols = c(columnList$yColumn, columnList$yFacetColumns)]/15
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

#' @inherit plotPKForest
#' @export
plotPKForestAbsoluteValuesWithVariance <- function(projectConfiguration,
                                                   onePlotConfig,
                                                   pkParameterDT,
                                                   pkParameterObserved = NULL,
                                                   aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles"),
                                                   percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1,3,5)],
                                                   scaleVectors = list(),
                                                   labelWrapWidth = 10,
                                                   vlineIntercept = NULL,
                                                   withTable = TRUE,
                                                   relWidths = NULL,
                                                   digitsToRound = 2,
                                                   digitsToShow = 2){
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
                           labelWrapWidth = labelWrapWidth)
  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestAbsoluteValuesWithCI <- function(projectConfiguration,
                                             onePlotConfig,
                                             pkParameterDT,
                                             pkParameterObserved = NULL,
                                             coefficientOfVariation = NULL,
                                             nObservationDefault,
                                             nBootstrap = 1000,
                                             statFun = c('geometric mean' = function(y) exp(mean(log(y[y>0])))),
                                             confLevel = 0.9,
                                             scaleVectors = list(),
                                             labelWrapWidth = 10,
                                             vlineIntercept = NULL,
                                             withTable = TRUE,
                                             relWidths = NULL,
                                             digitsToRound = 2,
                                             digitsToShow = 2){
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
                           relWidths = relWidths)


  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestRatiosWithVariance <- function(projectConfiguration,
                                           onePlotConfig,
                                           pkParameterDT,
                                           pkParameterObserved = NULL,
                                           aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles"),
                                           percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1,3,5)],
                                           nBootstrap = 1000,
                                           scaleVectors = list(),
                                           labelWrapWidth = 10,
                                           vlineIntercept = NULL,
                                           withTable = TRUE,
                                           relWidths = NULL,
                                           digitsToRound = 2,
                                           digitsToShow = 2){
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
                           nBootstrap = nBootstrap,
                           scaleVectors = scaleVectors,
                           vlineIntercept = vlineIntercept,
                           labelWrapWidth = labelWrapWidth)


  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestRatiosWithCI <- function(projectConfiguration,
                                     onePlotConfig,
                                     pkParameterDT,
                                     pkParameterObserved = NULL,
                                     coefficientOfVariation = NULL,
                                     nObservationDefault,
                                     nBootstrap = 1000,
                                     statFun = c('geometric mean' = function(y) exp(mean(log(y[y>0])))),
                                     confLevel = 0.9,
                                     scaleVectors = list(),
                                     labelWrapWidth = 10,
                                     vlineIntercept = c(1),
                                     withTable = TRUE,
                                     relWidths = NULL,
                                     digitsToRound = 2,
                                     digitsToShow = 2){
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
                           relWidths = relWidths)



  return(plotList)
}

# auxiliary  -------------
' Update Scale Vectors
#'
#' This function updates the scale vectors for simulated and observed data based on the provided input.
#'
#' @param scaleVectorsInput A list containing user-defined scale vectors for simulated and observed data.
#'        The list should have two components: `simulated` and `observed`, each containing a list of properties
#'        to modify the default scale vector settings.
#'
#' @return A list containing updated scale vectors for both simulated and observed data. Each component of the
#'         list includes properties such as `color`, `fill`, and `shape`.
#'
#' @keywords internal
updateScalevector <- function(scaleVectorsInput){

  scaleVectors <- list(
    simulated =
      list(
        color = 'black',
        fill = 'black',
        shape = 'circle filled'
    ),
    observed =
      list(
        color = 'darkgrey',
        fill = 'lightgrey',
        shape = 'triangle filled'
      )
  )
  for (f in names(scaleVectors)){
    if (!is.null(scaleVectorsInput[[f]])){
      scaleVectors[[f]] <- utils::modifyList(scaleVectors[[f]],
                                             scaleVectorsInput[[f]])
    }
  }

return(scaleVectors)

}
#' Filter Observed Parameters
#'
#' Filters observed PK parameters based on the provided configuration.
#'
#' @inheritParams plotPKForest
#' @return Filtered data table of observed PK parameters.
#' @keywords internal
filterParameterObserved <- function(pkParameterObserved,onePlotConfig){
  if (is.null(pkParameterObserved)) return(NULL)

  pkParameterObserved <-
    data.table::setnames(
      x = pkParameterObserved,
      old = c(
        'values',
        'minValue',
        'maxValue',
        'numberOfObservations',
        'parameter',
        'errorValues',
        'errorTypes'
      ),
      new = c(
        'x',
        'xMin',
        'xMax',
        'nObservations',
        'pkParameter',
        'xErrorValues',
        'xErrorTypes'
      ),
      skip_absent = TRUE
    )

  pkParameterObserved <- pkParameterObserved %>%
    merge(onePlotConfig[,c('dataGroupId','pkParameter','outputPathId')] %>%
            unique(),
          by.x = c('group','pkParameter','outputPathId'),
          by.y = c('dataGroupId','pkParameter','outputPathId'))

  return(pkParameterObserved)
}
#' Filter Simulated Parameters
#'
#' Filters simulated PK parameters based on the provided configuration.
#'
#' @inheritParams plotPKForest
#' @return Filtered data table of simulated PK parameters.
#' @keywords internal
filterParameterSimulated <- function(projectConfiguration,pkParameterDT,onePlotConfig,ratioMode,coefficientOfVariation,asCI){

  configToFilter <- onePlotConfig[,c('pkParameter','outputPathId','scenario','dataGroupId','referenceScenario')] %>%
    unique()

  switch(ratioMode,
         'none' = {
           pkParameterDT <- merge(configToFilter[,-'referenceScenario'],
                                  pkParameterDT,
                                  by.x = c('scenario','pkParameter','outputPathId'),
                                  by.y = c('scenarioName','parameter','outputPathId'))
         },
         'individualRatios' = {
           pkParameterDT <-
             merge(configToFilter,
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
             merge(configToFilter[,-'referenceScenario'],
                   pkParameterDT,
                   by.x = c('scenario','pkParameter','outputPathId'),
                   by.y = c('scenarioName','parameter','outputPathId')),
             merge(configToFilter[!(referenceScenario %in% configToFilter$scenario),-c('scenario','dataGroupId')],
                   pkParameterDT,
                   by.x = c('referenceScenario','pkParameter','outputPathId'),
                   by.y = c('scenarioName','parameter','outputPathId')) %>%
               setnames(old = 'referenceScenario', new = 'scenario') %>%
               dplyr::mutate(dataGroupId = NA)
           )

           tmp <- pkParameterDT[,c('scenario','pkParameter','outputPathId','displayUnitPKParameter')] %>%
             unique()
           if (any(duplicated(tmp[,c('scenario','pkParameter','outputPathId')]))){
             print(tmp[duplicated(tmp[,c('scenario','pkParameter','outputPathId')])])
             stop('Units are not consistent between scenario and referenceScenario')
           }
         }
  )

  return(pkParameterDT)
}
#' Prepare Data for PK Forest
#'
#' Prepares data for generating a PK forest plot.
#'
#' @param onePlotConfig Configuration for the plot.
#' @param pkParameterDT Data table containing PK parameter data.
#' @param pkParameterObserved Optional data table for observed PK parameters.
#' @param ratioMode Mode for ratio calculations.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param aggregationFun Function used for aggregation.
#' @param aggregationFlag Optional aggregation method.
#' @param nBootstrap Number of bootstrap samples.
#' @param nObservationDefault Default number of observations.
#' @param confLevel Confidence level for intervals.
#' @return A list of prepared data for the PK forest plot.
#' @keywords internal
prepareDataForPKForest <- function(
    onePlotConfig,
    pkParameterDT,
    pkParameterObserved,
    ratioMode,
    asCI,
    aggregationFun,
    aggregationFlag,
    nBootstrap,
    confLevel,
    nObservationDefault) {

  pkParameterDT <- addNObservations(pkParameterDT = pkParameterDT,
                                    pkParameterObserved = pkParameterObserved,
                                    nObservationDefault = nObservationDefault,
                                    asCI = asCI)
  plotData <- switch(ratioMode,
         'none' = {
           calculatePlotDataNone(pkParameterDT = pkParameterDT,
                                 asCI = asCI,
                                 aggregationFun = aggregationFun,
                                 nBootstrap = nBootstrap,
                                 confLevel = confLevel)
         },
         'individualRatios' = {
           calculatePlotDataIndividualRatios(pkParameterDT = pkParameterDT,
                                             asCI = asCI,
                                             aggregationFun = aggregationFun,
                                             nBootstrap = nBootstrap,
                                             confLevel = confLevel)
         },
         'ratioOfPopulation' = {
           calculatePlotDataRatioOfPopulation(onePlotConfig = onePlotConfig,
                                              pkParameterDT = pkParameterDT,
                                              asCI = asCI,
                                              aggregationFun = aggregationFun,
                                              aggregationFlag = aggregationFlag,
                                              nBootstrap = nBootstrap,
                                              confLevel = confLevel)
         }
  )


  plotData <- addObservedData(plotData = plotData,
                              pkParameterObserved = pkParameterObserved,
                              onePlotConfig = onePlotConfig)
  plotData <- addDescriptions(plotData = plotData,
                              onePlotConfig = onePlotConfig,
                              pkParameterDT = pkParameterDT)

  plotDataGroup <- split(plotData, by = ifelse(ratioMode == 'none', 'pkParameter', 'outputPathId'))

  return(plotDataGroup)
}
#' Add Observations to PK Parameter Data
#'
#' This function adds the number of observations to the PK parameter data table.
#'
#' @param pkParameterDT Data table containing PK parameter data.
#' @param pkParameterObserved Optional data table for observed PK parameters.
#' @param nObservationDefault Default number of observations.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @return Updated data table with observations added.
#' @keywords internal
addNObservations <- function(pkParameterDT, pkParameterObserved, nObservationDefault, asCI) {
  if (asCI) {
    if (!is.null(pkParameterObserved)) {
      pkParameterDT <- pkParameterDT %>%
        merge(pkParameterObserved[, c('group', 'outputPathId', 'pkParameter', 'nObservations')] %>% unique(),
              by.x = c('dataGroupId', 'outputPathId', 'pkParameter'),
              by.y = c('group', 'outputPathId', 'pkParameter'),
              all.x = TRUE,
              sort = FALSE)
      pkParameterDT[is.na(nObservations), nObservations := nObservationDefault]
    } else {
      pkParameterDT[, nObservations := nObservationDefault]
    }
  }
  return(pkParameterDT)
}
#' Calculate Plot Data for No Ratios
#'
#' This function calculates the plot data when no ratios are used.
#'
#' @param pkParameterDT Data table containing PK parameter data.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param aggregationFun Function used for aggregation.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @return Data table with calculated plot data for no ratios.
#' @keywords internal
calculatePlotDataNone <- function(pkParameterDT, asCI, aggregationFun, nBootstrap, confLevel) {
  if (asCI) {
    return(pkParameterDT[, as.list(getCIbyBootstrapping(y = value,
                                                        statFun = aggregationFun,
                                                        scenarioName = scenario,
                                                        pkParameter = pkParameter,
                                                        outputPathId = outputPathId,
                                                        nObservations = nObservations,
                                                        nBootstrap = nBootstrap,
                                                        confLevel = confLevel)),
                         by = c('pkParameter', 'outputPathId', 'scenario')])
  } else {
    return(getAggregatedVariance(dt = pkParameterDT,
                                 aggregationFun = aggregationFun,
                                 valueColumn = 'value',
                                 identifier = c('pkParameter', 'outputPathId', 'scenario'),
                                 direction = 'x'))
  }
}
#' Calculate Plot Data for Individual Ratios
#'
#' This function calculates the plot data for individual ratios.
#'
#' @param pkParameterDT Data table containing PK parameter data.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param aggregationFun Function used for aggregation.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @return Data table with calculated plot data for individual ratios.
#' @keywords internal
calculatePlotDataIndividualRatios <- function(pkParameterDT, asCI, aggregationFun, nBootstrap, confLevel) {
  if (asCI) {
    return(pkParameterDT[, as.list(getCIbyBootstrapping(y = ratio,
                                                        statFun = aggregationFun,
                                                        scenarioName = scenario,
                                                        pkParameter = pkParameter,
                                                        outputPathId = outputPathId,
                                                        nObservations = nObservations,
                                                        nBootstrap = nBootstrap,
                                                        confLevel = confLevel)),
                         by = c('pkParameter', 'outputPathId', 'scenario', 'referenceScenario')])
  } else {
    return(getAggregatedVariance(dt = pkParameterDT,
                                 aggregationFun = aggregationFun,
                                 valueColumn = 'ratio',
                                 identifier = c('scenario', 'pkParameter', 'outputPathId'),
                                 direction = 'x'))
  }
}

#' Calculate Plot Data for Ratio of Population
#'
#' This function calculates the plot data for the ratio of population.
#'
#' @param onePlotConfig Configuration for the plot.
#' @param pkParameterDT Data table containing PK parameter data.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param aggregationFun Function used for aggregation.
#' @param aggregationFlag Optional aggregation method.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @return Data table with calculated plot data for ratio of population.
#' @keywords internal
calculatePlotDataRatioOfPopulation <- function(onePlotConfig,pkParameterDT, asCI, aggregationFun, aggregationFlag,nBootstrap, confLevel) {
  if (asCI) {
    return(evaluateRatioByBootstrapping(onePlotConfig,
                                        pkParameterDT = pkParameterDT,
                                        aggregationFun = aggregationFun[[1]],
                                        aggregationFlag = names(aggregationFun),
                                        nBootstrap = nBootstrap,
                                        confLevel = confLevel))
  } else {
    return(evaluateRatioByBootstrapping(onePlotConfig = onePlotConfig,
                                        pkParameterDT = pkParameterDT,
                                        aggregationFun = aggregationFun,
                                        aggregationFlag = aggregationFlag,
                                        nBootstrap = nBootstrap,
                                        confLevel = confLevel))
  }
}
#' Add Observed Data to Plot Data
#'
#' This function adds the observed data to the plot data.
#'
#' @param plotData Data table with calculated plot data.
#' @param pkParameterObserved Optional data table for observed PK parameters.
#' @param onePlotConfig Configuration for the plot.
#' @return Updated plot data with observed data added.
#' @keywords internal
addObservedData <- function(plotData, pkParameterObserved, onePlotConfig) {
  plotData[,dataType := 'simulated']
  if (!is.null(pkParameterObserved)) {
    plotData <- rbind(plotData,
                      pkParameterObserved %>%
                        merge(onePlotConfig[, c('scenario', 'dataGroupId')] %>% unique(),
                              by.x = 'group',
                              by.y = 'dataGroupId') %>%
                        dplyr::select(any_of(names(plotData))),
                      fill = TRUE)
  }
  return(plotData)
}
#' Add Descriptions to Plot Data
#'
#' This function adds descriptions to the plot data.
#'
#' @param plotData Data table with calculated plot data.
#' @param onePlotConfig Configuration for the plot.
#' @param pkParameterDT Data table containing PK parameter data.
#'
#' @return Updated plot data with descriptions added.
#' @keywords internal
addDescriptions <- function(plotData, onePlotConfig,pkParameterDT) {
  plotData <- merge(plotData,
                    onePlotConfig[, c('scenario', 'scenarioShortName', 'scenarioGroup')] %>% unique(),
                    by = 'scenario')

  plotData$scenarioShortName <- factor(
    plotData$scenarioShortName,
    levels = unique(onePlotConfig$scenarioShortName),
    ordered = TRUE
  )

  #' Add Output Path Details to Plot Data
  plotData <- merge(plotData,
                    configEnv$outputPaths[, c('outputPathId', 'displayNameOutputs', 'displayUnit')] %>% unique(),
                    by = 'outputPathId')

  plotData$outputPathId <- factor(
    plotData$outputPathId,
    levels = configEnv$outputPaths[outputPathId %in% plotData$outputPathId, ]$outputPathId,
    ordered = TRUE
  )

  plotData[, plotTag := generatePlotTag(as.numeric(outputPathId))]

  #' Add PK Parameter Details to Plot Data
  plotData <- plotData %>%
    merge(pkParameterDT[, c('pkParameter', 'displayNamePKParameter', 'displayUnitPKParameter')] %>% unique(),
          by = c('pkParameter'))

  plotData$displayNamePKParameter <- factor(
    plotData$displayNamePKParameter,
    levels = unique(pkParameterDT[pkParameter %in% plotData$pkParameter]$displayNamePKParameter),
    ordered = TRUE
  )

  return(plotData)
}
#' Get Table Labels for PK Forest
#'
#' Generates table labels for the PK forest plot.
#'
#' @param plotData Data table containing plot data.
#' @param percentiles Percentiles for the plot.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param confLevel Confidence level for intervals.
#' @param ratioMode Mode for ratio calculations.
#' @return A named vector of table labels.
#' @keywords internal
getTableLabelsForPKForest <- function(plotData,percentiles,asCI,confLevel,ratioMode,aggregationFun){

  if (asCI){
    tableColumns <- c('x','xMin','xMax')
    tableLabels = c(ifelse(ratioMode == 'none','Value','Ratio'),
                    paste0(confLevel*100,'%\nCI lower'), paste0(confLevel*100,'%\nCI upper'))
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
#' Get Column Selection for PK Forest
#'
#' Selects columns for the PK forest plot based on the provided data and ratio mode.
#'
#' @param plotData Data table containing plot data.
#' @param ratioMode Mode for ratio calculations.
#' @return A list of selected columns for the plot.
#' @keywords internal
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

  if (uniqueN(plotData[[columnList$yColumn]]) == 1){
    columnList$xLabel <- paste(plotData[[columnList$yColumn]][1],columnList$xLabel)
    columnList$yColumn <- tail(columnList$yFacetColumns,1)
    columnList$yFacetColumns <- setdiff(columnList$yFacetColumns,columnList$yColumn)
  }


  if (uniqueN(plotData$plotTag) > 1){
    columnList$xFacetColumn = 'plotTag'
  } else {
    columnList$xFacetColumn = NULL
  }
  return(columnList)
}
#' Adjust PK Forest Plot Object
#'
#' Adjusts aesthetics of the PK forest plot object.
#'
#' @param plotObject The plot object to adjust.
#' @param scaleVectors A list defining colors, fills, and shapes for the plot.
#' @param vlineIntercept Optional vertical line intercept.
#' @return The adjusted plot object.
#' @keywords internal
adjustPkForestPlotObject <- function(plotObject,scaleVectors,vlineIntercept){
  plotObject <- plotObject +
    theme(legend.title = element_blank(),
          panel.grid.major.y = element_blank()) +
    scale_color_manual(values = unlist(lapply(scaleVectors, getElement, 'color'))) +
    scale_fill_manual(values = unlist(lapply(scaleVectors, getElement, 'fill'))) +
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
      theme(legend.position = 'bottom',
            legend.direction = 'horizontal')
  }

  return(plotObject)
}
#' Add Inter-Occasional Variability
#'
#' Adds inter-occasional variability to the given values.
#'
#' @param value Numeric vector of values.
#' @param scenarioName Name of the scenario.
#' @param pkParameter Name of the PK parameter.
#' @param outputPathId ID for the output path.
#' @param sigma Standard deviation for the variability.
#' @return Numeric vector with added variability.
#' @keywords internal
addInterOccasionalVariability <- function(value, scenarioName, pkParameter, outputPathId,sigma) {

  # Set seed
  setSeedForPKParameter(scenarioName, pkParameter, outputPathId)
  valueIOV = value * exp(rnorm(n = length(value), mean = 0, sd = sigma))

  return(valueIOV) # Return the new value
}
#' Set Seed for PK Parameter
#'
#' Sets the seed for random number generation based on the provided parameters.
#'
#' @param scenarioName Name of the scenario.
#' @param pkParameter Name of the PK parameter.
#' @param outputPathId ID for the output path.
#' @param referenceScenarion Optional reference scenario name.
#' @return NULL (invisible).
#' @keywords internal
setSeedForPKParameter <- function(scenarioName, pkParameter, outputPathId,referenceScenarion = NULL){
  if (!is.null(referenceScenarion)){
    seedString <- paste(scenarioName[1], pkParameter[1], outputPathId[1],sep = "_")
  } else {
    seedString <- paste(scenarioName[1], referenceScenarion[1],pkParameter[1], outputPathId[1],sep = "_")
  }
  numericSeed <- sum(utf8ToInt(seedString))
  set.seed(numericSeed)
  return(invisible())
}
#' Get Confidence Intervals by Bootstrapping
#'
#' Calculates confidence intervals using bootstrapping.
#'
#' @param y Numeric vector of values.
#' @param statFun Function used for aggregation.
#' @param scenarioName Name of the scenario.
#' @param pkParameter Name of the PK parameter.
#' @param outputPathId ID for the output path.
#' @param nObservations Number of observations.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @return A list containing the calculated confidence intervals.
#' @keywords internal
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
#' Evaluate Ratio by Bootstrapping
#'
#' Evaluates the ratio using bootstrapping.
#'
#' @param onePlotConfig Configuration for the plot.
#' @param pkParameterDT Data table containing PK parameter data.
#' @param aggregationFun Function used for aggregation.
#' @param aggregationFlag Optional aggregation method.
#' @param nBootstrap Number of bootstrap samples.
#' @param nObservations Number of samples to draw per bootstrap.
#' @param confLevel Confidence level for intervals.
#' @return A data.table containing the evaluated ratios.
#' @keywords internal
evaluateRatioByBootstrapping <-
  function(onePlotConfig,
           pkParameterDT,
           aggregationFun,
           aggregationFlag,
           nBootstrap,
           confLevel) {

  onePlotConfig <- onePlotConfig %>%
    merge(pkParameterDT[,c('outputPathId','pkParameter','scenario')] %>%  unique(),
          by = c('outputPathId','pkParameter','scenario'))

  # Apply the function to each unique combination of PKParameter and outputPathId
  resultList <-
    lapply(split(
      onePlotConfig[!is.na(referenceScenario), c('outputPathId',
                                                 'pkParameter',
                                                 'scenario',
                                                 'referenceScenario')],
      by = c('outputPathId', 'pkParameter', 'scenario')
    ), function(dt) {
      dtDefault <- merge(dt,
                       pkParameterDT,
                       by = c('outputPathId','pkParameter','scenario'))

      dtReference <- merge(dt,
                         pkParameterDT,
                         by.x = c('outputPathId','pkParameter','referenceScenario'),
                         by.y = c('outputPathId','pkParameter','scenario'))

    if (aggregationFlag == ospsuite::DataErrorType$GeometricStdDev & is.null(confLevel)){
      # add quantities which are calculated analytical
      lValues = log( dtDefault[!is.na(value) & value>0]$value)
      lReferenceValues = log( dtReference[!is.na(value) & value>0]$value)
      geomean <- exp(mean(lValues) - mean(lReferenceValues))
      geoSd <- exp(sqrt((sd(lValues))^2 + (sd(lReferenceValues))^2))

      returnList <- list(x = geomean,
                         xMin = geomean/geoSd,
                         xMax = geomean*geoSd,
                         xErrorType = aggregationFlag)
    } else {
      setSeedForPKParameter(scenarioName = dtDefault$scenario[1],referenceScenarion = dtReference$scenario[1],
                            outputPathId = dtDefault$outputPathId[1],pkParameter = dtDefault$pkParameter[1])

      result <- aggregateRatiosByBootstrapping(nBootstrap = nBootstrap,
                                               sampleSize = dtDefault$nObservations[1],
                                               value =  dtDefault[!is.na(value)]$value,
                                               valueReference = dtReference[!is.na(value)]$value,
                                               aggregationFun = aggregationFun,
                                               confLevel = confLevel)

      if (is.null(confLevel)){
        if (aggregationFlag == ospsuite::DataErrorType$ArithmeticStdDev){
          returnList <- list(x = result$yValues,
                             xMin = result$yValues - result$yErrorValues,
                             xMax = result$yValues + result$yErrorValues,
                             xErrorType = result$yErrorType)
        } else {
          returnList <- list(x = result$yValues,
                             xMin = result$yMin,
                             xMax = result$yMax,
                             xErrorType = result$yErrorType)
        }
      } else {
        returnList <- list(x = result$value,
                           xMin =  result$value_lower,
                           xMax =  result$value_upper,
                           xErrorType = paste0(names(aggregationFun),'|',100*confLevel,'% confidence interval'))
      }

    }

    # add identifier
      returnList[['scenario']] <- dtDefault$scenario[1]
      returnList[['outputPathId']] <- dtDefault$outputPathId[1]
      returnList[['pkParameter']] <- dtDefault$pkParameter[1]
    return(returnList)

  })

  # Combine results into a single data.table
  return(rbindlist(resultList))

}
#' Get Caption for Forest Plot
#'
#' Generates a caption for the forest plot based on the provided data.
#'
#' @param plotData Data table containing plot data.
#' @param xScale Scale of the x-axis.
#' @param plotCaptionAddon Additional caption text.
#' @param ratioMode Mode for ratio calculations.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @return A string containing the caption for the plot.
#' @keywords internal
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

  captiontext <- addCaptionTextAddon(captiontext,plotCaptionAddon)

  return(captiontxt)

}
#' Get Footnote Lines for Forest Plots
#'
#' Generates footnote lines for the forest plot.
#'
#' @param plotData Data table containing plot data.
#' @param ratioMode Mode for ratio calculations.
#' @param asCI Logical indicating if confidence intervals should be calculated.
#' @param dtDataReference Optional data reference.
#' @return A character vector containing footnote lines.
#' @keywords internal
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

# validation ---------
#' Validate PK Forest Absolute Values with Variance Configuration
#'
#' Validates the configuration for PK forest plots with absolute values and variance.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#' @export
validatePKForestAbsoluteValuesWithVarianceConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)
    return(invisible())
  }
#' Validate PK Forest Absolute Values with Confidence Intervals Configuration
#'
#' Validates the configuration for PK forest plots with absolute values and confidence intervals.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#' @export
validatePKForestAbsoluteValuesWithCIConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)
    return(invisible())
  }
#' Validate PK Forest Ratios with Variance Configuration
#'
#' Validates the configuration for PK forest plots with ratios and variance.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#' @export
validatePKForestRatiosWithVarianceConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)

    # check if reference Scenarios are there
    validateExistenceOfReferenceForRatio(configTablePlots = configTablePlots)
    return(invisible())
  }
#' Validate PK Forest Ratios with Confidence Intervals Configuration
#'
#' Validates the configuration for PK forest plots with ratios and confidence intervals.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#' @export
validatePKForestRatiosWithCIConfig <-
  function(configTable, pkParameterDT, ...){
    configTablePlots <- validatePKForestConfigTable(configTable, pkParameterDT, ...)

    # check if reference Scenarios are there
    validateExistenceOfReferenceForRatio(configTablePlots = configTablePlots)
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
  dotArgs = list(...)
  pkParameterObserved <- dotArgs$pkParameterObserved

  if (!is.null(pkParameterObserved)){
    checkmate::assertDataTable(pkParameterObserved)
    if (!DATACLASS$pkAggregated %in% unique(pkParameterObserved$dataClass))
      stop('Please provide aggregated observed PK-Parameter data for this kind of plot ')
  }

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
      ),
      outputPathId = list(
        cols = c("dataGroupId"),
        allowedValues = unique(pkParameterObserved$group),
        splitAllowed = FALSE
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
#' Validate Bootstrapping Inputs
#'
#' Validates the inputs for bootstrapping in PK forest plots.
#'
#' @param nObservationDefault Default number of observations.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @param coefficientOfVariation Coefficient of variation for the data.
#' @param statFun Statistical function for bootstrapping.
#' @param ratioMode Mode for ratio calculations.
#' @return NULL (invisible).
#' @keywords internal
validateBootstrappingInputs <- function(nObservationDefault,nBootstrap,confLevel,coefficientOfVariation,statFun,ratioMode){
  checkmate::assertIntegerish(nObservationDefault, lower = 0, len = 1)
  checkmate::assertIntegerish(nBootstrap, lower = 0, len = 1)
  checkmate::assertDouble(confLevel, lower = 0, upper = 1, len = 1, finite = TRUE)
  checkmate::assertFunction(statFun[[1]])
  checkmate::assertNamed(statFun)

  if (ratioMode == 'ratioOfPopulation'){
    if (!is.null(coefficientOfVariation))
      warning('Ratio is calculated as ratio of different populations, interoccasional variability will be therefore neglected')
  } else if (ratioMode == 'individualRatios'){
    checkmate::assertDouble(coefficientOfVariation, lower = 0, len = 1, finite = TRUE)
  }

}
#' Validate Common Inputs
#'
#' Validates common inputs for PK forest plotting functions.
#'
#' @param pkParameterDT Data table containing PK parameter data.
#' @param pkParameterObserved Optional data table for observed PK parameters.
#' @param scaleVectors A list defining colors, fills, and shapes for the plot.
#' @param labelWrapWidth Width for wrapping labels.
#' @param vlineIntercept Optional vertical line intercept.
#' @return NULL (invisible).
#' @keywords internal
validateCommonInputs <- function(pkParameterDT,
                     pkParameterObserved,
                     scaleVectors,
                     labelWrapWidth,
                     vlineIntercept){

  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayNamePKParameter", "displayUnitPKParameter"))
  # Check if scaleVectors is a list
  checkmate::assertList(scaleVectors)
  if (length(scaleVectors) > 0){
    # Check if it contains the expected names
    checkmate::assertNames(names(scaleVectors), subset.of = c("simulated", "observed"))

    # Check each entry to ensure they are lists with the correct names
    for (entry in scaleVectors) {
      checkmate::assertList(entry, names = "named")
      checkmate::assertNames(names(entry), subset.of = c("color", "fill", "shape"))
    }
  }

  checkmate::assertNumeric(labelWrapWidth,lower = 0, len = 1)
  checkmate::assertNumeric(vlineIntercept,null.ok = TRUE)
}



# support usability --------------------
#' Add Default Configuration for PK Forest Plots
#'
#' Adds default configurations for forest plots to the `Plots.xlsx` configuration file.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param sheetName Name of the sheet to create.
#' @param overwrite Logical indicating if existing data should be overwritten.
#' @return NULL (invisible).
#'
#' #' @seealso
#' \code{\link{plotPKForestAbsoluteValuesWithVariance}}, \code{\link{plotPKForestAbsoluteValuesWithCI}},
#' \code{\link{plotPKForestRatiosWithVariance}}, \code{\link{plotPKForestRatiosWithCI}},
#
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
