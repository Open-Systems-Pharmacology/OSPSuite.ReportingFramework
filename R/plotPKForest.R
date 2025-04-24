#' @title PK Forest Plots
#'
#' @description
#' This is one of a group of functions which generates PK forest plots for pharmacokinetic (PK) parameters, providing visualizations
#' that can include absolute values or ratios, along with options for displaying variance
#' or point estimates. These functions allow for the comparison of simulated and observed
#' PK parameter data across different scenarios, aiding in the interpretation of variability
#' and uncertainty in the estimates.
#'
#' Available functions are:
#'
#' \describe{
#'   \item{\code{plotPKForestAggregatedAbsoluteValues}}{Generates a PK forest plot displaying absolute values along with variance.}
#'   \item{\code{plotPKForestPointEstimateOfAbsoluteValues}}{Generates a PK forest plot displaying point estimates.}
#'   \item{\code{plotPKForestAggregatedRatios}}{Generates a PK forest plot displaying ratios of PK parameters along with variance.}
#'   \item{\code{plotPKForestPointEstimateOfRatios}}{Generates a PK forest plot displaying point estimates of ratios of PK parameters.}
#' }
#'
#'
#' @details
#'
#' For the ratio calculation, the function distinguishes between two cases:
#'
#'
#' \describe{
#'   \item{Case 1:}{If the scenarios being compared are based on the \strong{same populations}
#'   (same `PopulationId` in scenario configuration), the plots will show summary statistics of
#'   individual ratios.}
#'   \item{Case 2:}{If the scenarios are based on \strong{different populations}, the plots
#'   will display the ratio of summary statistics. This ratio is only available for \code{plotPKForestPointEstimateOfRatios}}
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
#'  (for `plotPKForestAggregatedAbsoluteValues` and `plotPKForestAggregatedRatios`):
#' @param percentiles A numeric vector specifying which percentiles to calculate
#'   and display in the plot, only relevant if `aggregationFlag = "Percentiles"`
#'   (for `plotPKForestAggregatedAbsoluteValues` and `plotPKForestAggregatedRatios`):
#' @param statFun A named list of functions used for statistical aggregation, typically
#'   including the geometric mean function for bootstrapping.
#'   This must be a function accepting as input a numeric vector and returning a numeric value.
#'   The input is formatted as a named list, the name is used in plot legends and captions
#'    c('geometric mean' = function(y) exp(mean(log(y[y>0]))))
#'  (for `plotPKForestPointEstimateOfAbsoluteValues` and `plotPKForestPointEstimateOfRatios`)
#' @param confLevel A numeric value between 0 and 1 indicating the desired confidence
#'   level for the intervals (e.g., 0.9 for 90% confidence).
#'  (for `plotPKForestPointEstimateOfAbsoluteValues` and `plotPKForestPointEstimateOfRatios`)
#' @param nBootstrap An integer indicating the number of bootstrap samples to draw
#'   for calculating confidence intervals.
#'   (for `plotPKForestPointEstimateOfAbsoluteValues`, `plotPKForestPointEstimateOfRatios`)
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
#' @param asPointeEstimate Logical indicating if confidence intervals should be calculated.
#' @param ratioMode Mode for ratio calculations.
#'
#' @return A list containing the generated plot objects.
#'
#' @seealso
#' \code{\link{plotPKForestAggregatedAbsoluteValues}}, \code{\link{plotPKForestPointEstimateOfAbsoluteValues}},
#' \code{\link{plotPKForestAggregatedRatios}}, \code{\link{plotPKForestPointEstimateOfRatios}},
#' \code{\link{addDefaultConfigForPKForestPlots}}
#'
#' @keywords internal
plotPKForest <- function(projectConfiguration,
                         onePlotConfig,
                         pkParameterDT,
                         pkParameterObserved,
                         ratioMode,
                         asPointeEstimate,
                         aggregationFlag = NULL,
                         aggregationFun,
                         percentiles = NULL,
                         confLevel = NULL,
                         nBootstrap = 100,
                         vlineIntercept,
                         withTable = TRUE,
                         relWidths = NULL,
                         digitsToRound = 2,
                         digitsToShow = 2,
                         scaleVectors,
                         labelWrapWidth = 2) {
  # initialize to avoid linter messages
  scenarioGroup <- referenceScenario <- NULL

  # separates the comma separate inputs to rows
  onePlotConfig <- separateAndTrim(data = onePlotConfig, columnName = "outputPathIds") %>%
    separateAndTrim(columnName = "pkParameters")
  # use empty string for grouping
  onePlotConfig[is.na(scenarioGroup), scenarioGroup := ""]
  # scenarios without referenceScenario  are ignored in ratio mode
  if (ratioMode != "none") onePlotConfig <- onePlotConfig[!is.na(referenceScenario)]

  onePlotConfig <-
    merge(onePlotConfig,
      unique(pkParameterDT[, c("scenario", "pkParameter", "outputPathId")]),
      by = c("scenario", "pkParameter", "outputPathId")
    )

  scaleVectors <- updateScalevector(scaleVectorsInput = scaleVectors)
  # switch to naming convention of forest plot and filter for relevant data
  pkParameterObserved <- filterParameterObserved(pkParameterObserved, onePlotConfig)

  # filter for relevant data and calculate ratio if necessary
  pkParameterFiltered <-
    filterParameterSimulated(
      projectConfiguration = projectConfiguration,
      pkParameterDT = pkParameterDT,
      onePlotConfig = onePlotConfig,
      ratioMode = ratioMode,
      asPointeEstimate = asPointeEstimate
    )
  plotDataGroup <- prepareDataForPKForest(
    onePlotConfig = onePlotConfig,
    pkParameterFiltered = pkParameterFiltered,
    pkParameterObserved = pkParameterObserved,
    ratioMode = ratioMode,
    asPointeEstimate = asPointeEstimate,
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    aggregationFun = aggregationFun,
    aggregationFlag = aggregationFlag
  )

  plotList <- list()
  for (groupName in names(plotDataGroup)) {
    columnList <-
      getColumnSelectionForPKForest(plotData = plotDataGroup[[groupName]], ratioMode = ratioMode)

    tableLabels <-
      getTableLabelsForPKForest(
        plotData = plotDataGroup[[groupName]]
      )

    mapping <-
      getMappingForForestPlots(
        plotData = plotDataGroup[[groupName]],
        columnList = columnList)

    for (xScale in splitInputs(onePlotConfig$xScale[1])) {
      combinedObject <-
        plotForest(
          plotData = plotDataGroup[[groupName]],
          mapping = mapping,
          xLabel = columnList$xLabel,
          yFacetColumns = columnList$yFacetColumns,
          xFacetColumn = columnList$xFacetColumn,
          xscale = xScale,
          xscale.args = getXorYlimits(
            onePlotConfig = onePlotConfig,
            xOryScale = xScale,
            direction = "x"
          ),
          tableColumns = names(tableLabels),
          tableLabels = tableLabels,
          labelWrapWidth = labelWrapWidth,
          digitsToRound = digitsToRound,
          digitsToShow = digitsToShow,
          withTable = withTable,
          facetScales = onePlotConfig$facetScale[1]
        )

      combinedObject <-
        adjustPkForestPlotObject(
          combinedObject = combinedObject,
          scaleVectors = scaleVectors,
          vlineIntercept = vlineIntercept,
          tableLabels = tableLabels
        )
      if (!is.null(relWidths)) {
        combinedObject$relWidths <- relWidths
      } else if (uniqueN(plotDataGroup[[groupName]]$dataType) > 1) {
        combinedObject$relWidths <- c(5, 2)
      }

      combinedObject <- addPrecisisonWatermark(
        combinedObject = combinedObject,
        asPointeEstimate = asPointeEstimate,
        plotData = plotDataGroup[[groupName]]
      )

      # Prepare for export
      combinedObject <- setExportAttributes(
        object = combinedObject,
        caption = getCaptionForForestPlot(
          plotData = plotDataGroup[[groupName]],
          xScale = xScale,
          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
          ratioMode = ratioMode
        ),
        footNoteLines =
          getFootnoteLinesForForrestPlots(
            plotData = plotDataGroup[[groupName]],
            ratioMode = ratioMode,
            asPointeEstimate = asPointeEstimate,
            dtDataReference = NULL
          ),
        exportArguments =
          list(
            width = 12,
            heightToWidth = plotDataGroup[[groupName]][, uniqueN(.SD),
              .SDcols = c(
                columnList$yColumn, # nolint identation
                columnList$yFacetColumns
              )
            ] / 15
          )
      )

      # Create figure key and store plot
      figureKey <- paste(onePlotConfig$plotName[1],
        groupName,
        ifelse(xScale == "log", "log", "linear"),
        sep = "-"
      )
      plotList[[figureKey]] <- combinedObject
    }
  }

  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestAggregatedAbsoluteValues <- function(projectConfiguration,
                                                 onePlotConfig,
                                                 pkParameterDT,
                                                 pkParameterObserved = NULL,
                                                 aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles", "Custom"),
                                                 percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1, 3, 5)],
                                                 customFunction = NULL,
                                                 scaleVectors = list(),
                                                 labelWrapWidth = 10,
                                                 vlineIntercept = NULL,
                                                 withTable = TRUE,
                                                 relWidths = NULL,
                                                 digitsToRound = 2,
                                                 digitsToShow = 2) {
  validateCommonInputs(
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    scaleVectors = scaleVectors,
    labelWrapWidth = labelWrapWidth,
    vlineIntercept = vlineIntercept
  )

  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction, legendsize = 3)

  plotList <-
    plotPKForest(projectConfiguration,
      onePlotConfig = onePlotConfig,
      pkParameterDT = pkParameterDT,
      ratioMode = "none",
      asPointeEstimate = FALSE,
      pkParameterObserved = pkParameterObserved,
      aggregationFlag = aggregationFlag,
      aggregationFun = aggregationFun,
      percentiles = percentiles,
      scaleVectors = scaleVectors,
      vlineIntercept = vlineIntercept,
      labelWrapWidth = labelWrapWidth
    )
  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestPointEstimateOfAbsoluteValues <- function(projectConfiguration,
                                                      onePlotConfig,
                                                      pkParameterDT,
                                                      pkParameterObserved = NULL,
                                                      statFun = c("geometric mean" = function(y) exp(mean(log(y[y > 0])))),
                                                      confLevel = 0.9,
                                                      nBootstrap = 100,
                                                      scaleVectors = list(),
                                                      labelWrapWidth = 10,
                                                      vlineIntercept = NULL,
                                                      withTable = TRUE,
                                                      relWidths = NULL,
                                                      digitsToRound = 2,
                                                      digitsToShow = 2) {
  ratioMode <- "none"

  validateCommonInputs(
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    scaleVectors = scaleVectors,
    labelWrapWidth = labelWrapWidth,
    vlineIntercept = vlineIntercept
  )

  validatePointEstimateInputs(
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    statFun = statFun
  )


  plotList <- plotPKForest(
    projectConfiguration = projectConfiguration,
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    ratioMode = ratioMode,
    asPointeEstimate = TRUE,
    aggregationFlag = NULL,
    aggregationFun = statFun,
    percentiles = NULL,
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    vlineIntercept = vlineIntercept,
    scaleVectors = scaleVectors,
    digitsToRound = digitsToRound,
    digitsToShow = digitsToShow,
    labelWrapWidth = labelWrapWidth,
    relWidths = relWidths
  )


  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestAggregatedRatios <- function(projectConfiguration,
                                         onePlotConfig,
                                         pkParameterDT,
                                         pkParameterObserved = NULL,
                                         aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles", "Custom"),
                                         percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1, 3, 5)],
                                         customFunction = NULL,
                                         scaleVectors = list(),
                                         labelWrapWidth = 10,
                                         vlineIntercept = NULL,
                                         withTable = TRUE,
                                         relWidths = NULL,
                                         digitsToRound = 2,
                                         digitsToShow = 2) {
  validateCommonInputs(
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    scaleVectors = scaleVectors,
    labelWrapWidth = labelWrapWidth,
    vlineIntercept = vlineIntercept
  )

  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction, legendsize = 3)

  # as this essential, do this validation here and not in validatePKForestAggregatedRatios
  # within a custom function plotPKForestAggregatedRatios may be called without validatePKForestAggregatedRatios
  validateIsCrossOverStudy(
    configTablePlots = onePlotConfig,
    pkParameterDT = pkParameterDT
  )

  plotList <-
    plotPKForest(projectConfiguration,
      onePlotConfig = onePlotConfig,
      pkParameterDT = pkParameterDT,
      ratioMode = "individualRatios",
      asPointeEstimate = FALSE,
      pkParameterObserved = pkParameterObserved,
      aggregationFlag = aggregationFlag,
      aggregationFun = aggregationFun,
      percentiles = percentiles,
      scaleVectors = scaleVectors,
      vlineIntercept = vlineIntercept,
      labelWrapWidth = labelWrapWidth
    )


  return(plotList)
}

#' @inherit plotPKForest
#' @export
plotPKForestPointEstimateOfRatios <- function(projectConfiguration,
                                              onePlotConfig,
                                              pkParameterDT,
                                              pkParameterObserved = NULL,
                                              nBootstrap = 1000,
                                              confLevel = 0.9,
                                              statFun = c("geometric mean" = function(y) exp(mean(log(y[y > 0])))),
                                              scaleVectors = list(),
                                              labelWrapWidth = 10,
                                              vlineIntercept = c(1),
                                              withTable = TRUE,
                                              relWidths = NULL,
                                              digitsToRound = 2,
                                              digitsToShow = 2) {
  validateCommonInputs(
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    scaleVectors = scaleVectors,
    labelWrapWidth = labelWrapWidth,
    vlineIntercept = vlineIntercept
  )

  ratioMode <- getRatioMode(
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    asRatio = TRUE
  )

  validatePointEstimateInputs(
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    statFun = statFun
  )

  plotList <- plotPKForest(
    projectConfiguration = projectConfiguration,
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    pkParameterObserved = pkParameterObserved,
    ratioMode = ratioMode,
    asPointeEstimate = TRUE,
    aggregationFlag = NULL,
    aggregationFun = statFun,
    percentiles = NULL,
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    vlineIntercept = vlineIntercept,
    scaleVectors = scaleVectors,
    digitsToRound = digitsToRound,
    digitsToShow = digitsToShow,
    labelWrapWidth = labelWrapWidth,
    relWidths = relWidths
  )

  return(plotList)
}

# auxiliary  -------------
#' Update Scale Vectors
#'
#' This function updates the scale vectors for simulated and observed data based on the provided input.
#'
#' @inheritParams plotPKForest
#'
#' @return A list containing updated scale vectors for both simulated and observed data. Each component of the
#'         list includes properties such as `color`, `fill`, and `shape`.
#'
#' @keywords internal
updateScalevector <- function(scaleVectorsInput) {
  scaleVectors <- list(
    simulated =
      list(
        color = "black",
        fill = "black",
        shape = "circle filled"
      ),
    observed =
      list(
        color = "darkgrey",
        fill = "lightgrey",
        shape = "triangle filled"
      )
  )
  for (f in names(scaleVectors)) {
    if (!is.null(scaleVectorsInput[[f]])) {
      scaleVectors[[f]] <- utils::modifyList(
        scaleVectors[[f]],
        scaleVectorsInput[[f]]
      )
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
filterParameterObserved <- function(pkParameterObserved, onePlotConfig) {
  if (is.null(pkParameterObserved)) {
    return(NULL)
  }

  # use copy so tha data.table outside stay unchanged
  pkParameterObserved <- copy(pkParameterObserved)
  pkParameterObserved <-
    data.table::setnames(
      x = pkParameterObserved,
      old = c(
        "values",
        "minValue",
        "maxValue",
        "errorValues",
        "errorType",
        "group"
      ),
      new = c(
        "xValues",
        "xMin",
        "xMax",
        "xErrorValues",
        "xErrorType",
        "dataGroupId"
      ),
      skip_absent = TRUE
    )

  pkParameterObserved <- pkParameterObserved %>%
    merge(
      onePlotConfig[, c("dataGroupId", "pkParameter", "outputPathId")] %>%
        unique(),
      by = c("dataGroupId", "pkParameter", "outputPathId")
    )

  return(pkParameterObserved)
}
#' Filter Simulated Parameters
#'
#' Filters simulated PK parameters based on the provided configuration.
#'
#' @inheritParams plotPKForest
#' @return Filtered data table of simulated PK parameters.
#' @keywords internal
filterParameterSimulated <- function(projectConfiguration, pkParameterDT, onePlotConfig, ratioMode, coefficientOfVariation, asPointeEstimate) {
  if (ratioMode != "ratioOfPopulation") {
    pkParameterFiltered <- mergePKParameterWithConfigTable(
      onePlotConfig = onePlotConfig[, c("pkParameter", "outputPathId", "scenario", "referenceScenario")] %>%
        unique(),
      pkParameterDT = pkParameterDT,
      asRatio = ratioMode == "individualRatios"
    )
  } else {
    pkParameterFiltered <- list(
      base = mergePKParameterWithConfigTable(
        onePlotConfig = onePlotConfig[, c("pkParameter", "outputPathId", "scenario")] %>%
          unique(),
        pkParameterDT = pkParameterDT
      ),
      reference = mergePKParameterWithConfigTable(
        onePlotConfig = onePlotConfig[, c("pkParameter", "outputPathId", "referenceScenario")] %>%
          unique() %>%
          setnames(old = "referenceScenario", new = "scenario"),
        pkParameterDT = pkParameterDT
      )
    )
  }

  return(pkParameterFiltered)
}
#' Prepare Data for PK Forest
#'
#' Prepares data for generating a PK forest plot.
#'
#' @param onePlotConfig Configuration for the plot.
#' @param pkParameterDT Data table containing PK parameter data.
#' @param pkParameterObserved Optional data table for observed PK parameters.
#' @param ratioMode Mode for ratio calculations.
#' @param asPointeEstimate Logical indicating if confidence intervals should be calculated.
#' @param aggregationFun Function used for aggregation.
#' @param aggregationFlag Optional aggregation method.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @return A list of prepared data for the PK forest plot.
#' @keywords internal
prepareDataForPKForest <- function(
    onePlotConfig,
    pkParameterFiltered,
    pkParameterObserved,
    ratioMode,
    asPointeEstimate,
    nBootstrap,
    confLevel,
    aggregationFun,
    aggregationFlag) {
  if (asPointeEstimate) {
    plotData <- aggregatePointEstimate(
      pkParameterFiltered = pkParameterFiltered,
      onePlotConfig = onePlotConfig,
      ratioMode = ratioMode,
      nBootstrap = nBootstrap,
      confLevel = confLevel,
      aggregationFun = aggregationFun,
      aggregationFlag = aggregationFlag
    )
    # reduce to datatable for description setting
    if (ratioMode == "ratioOfPopulation") {
      pkParameterFiltered <- pkParameterFiltered[[1]]
    }
  } else {
    plotData <- getAggregatedVariance(
      dt = pkParameterFiltered,
      aggregationFun = aggregationFun,
      valueColumn = "value",
      identifier = c("pkParameter", "outputPathId", "scenario"),
      direction = "x"
    )
  }

  plotData <- addObservedData(
    plotData = plotData,
    pkParameterObserved = pkParameterObserved,
    onePlotConfig = onePlotConfig
  )

  plotData <- addDescriptions(
    plotData = plotData,
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterFiltered
  )

  # Split data based on ratioMode
  plotDataGroup <- split(plotData, by = ifelse(ratioMode == "none", "pkParameter", "outputPathId"))

  plotDataGroup <- lapply(plotDataGroup, function(dataGroup) {
    adjustForestDataPerGroup(
      dataGroup = dataGroup,
      onePlotConfig = onePlotConfig
    )
  })

  return(plotDataGroup)
}
#' Adjust Forest Data Per Group
#'
#' Processes a data group to ensure unique scenarios and generates plot tags.
#'
#' This function checks the uniqueness of scenarios within the provided data group,
#' modifies the `outputPathId` to be a factor with specified levels, and generates
#' plot tags based on the `outputPathId`.
#'
#' @param dataGroup A data table representing a group of plot data.
#' @param onePlotConfig Configuration for the plot, containing metadata like plot names.
#' @return A modified data table with updated `outputPathId` as a factor and generated `plotTag`.
#' @keywords internal
adjustForestDataPerGroup <- function(dataGroup, onePlotConfig) {
  # initialize to avoid linter messages
  outputPathId <- plotTag <- NULL
  tmp <- dataGroup[, .N, by = c(
    "scenarioShortName",
    "scenarioGroup",
    "outputPathId",
    "pkParameter",
    "dataType"
  )]
  if (any(tmp$N > 1)) {
    stop("Scenarios are not unique regarding scenarioShortName and scenarioGroup.
           Check configuration for: ", onePlotConfig$plotName[1],)
  }
  if (uniqueN(dataGroup[['xErrorType']]) > 1) {
    stop("Scenarios are not unique regarding aggregation for observed and simulated data.
           Check errorType column for data relevant for plot: ", onePlotConfig$plotName[1],
         "errorTypes are: ",paste(unique(dataGroup[['xErrorType']]),collapse = ', '))
  }

  dataGroup$outputPathId <- factor(
    dataGroup$outputPathId,
    levels = configEnv$outputPaths[outputPathId %in% dataGroup$outputPathId, ]$outputPathId,
    ordered = TRUE
  )
  if (all(is.na(dataGroup$xMin))) {
    dataGroup[, `:=`(xMin = NULL, xMax = NULL)]
  }

  dataGroup <-
    dataGroup[, plotTag := generatePlotTag(as.numeric(outputPathId))]


  return(dataGroup)
}
#' Aggregate Point Estimates
#'
#' Aggregates point estimates from the filtered PK parameter data based on the specified ratio mode.
#'
#' @param pkParameterFiltered A list of data tables containing filtered PK parameter data.
#' @param onePlotConfig Configuration for the plot.
#' @param ratioMode A character string indicating the mode for ratio calculations.
#' @param nBootstrap Integer specifying the number of bootstrap samples.
#' @param confLevel Numeric value for the confidence level of the intervals.
#' @param aggregationFun Function used for aggregation.
#' @param aggregationFlag Optional aggregation method.
#' @return A data table containing the aggregated point estimates and confidence intervals.
#' @keywords internal
aggregatePointEstimate <- function(pkParameterFiltered,
                                   onePlotConfig,
                                   ratioMode,
                                   nBootstrap,
                                   confLevel,
                                   aggregationFun,
                                   aggregationFlag) {
  # initialize to avoid linter messages
  xValues <- xMin <- xMax <- xValues.reference <- xMin.reference <- xMax.reference <- NULL # nolint
  if (ratioMode == "ratioOfPopulation") {
    results <- lapply(
      pkParameterFiltered,
      function(dt) {
        calculateAggregationWithCIBYGroup(
          dt = dt,
          aggregationFun = aggregationFun,
          confLevel = confLevel,
          nBootstrap = nBootstrap,
          valueColumn = "value",
          identifier = c("pkParameter", "outputPathId", "scenario"),
          direction = "x"
        ) %>%
          checkPrecision()
      }
    )
    plotData <- unique(onePlotConfig[, c("pkParameter", "outputPathId", "scenario", "referenceScenario")]) %>%
      merge(results[["base"]],
        by = c("scenario", "pkParameter", "outputPathId")
      ) %>%
      merge(results[["reference"]],
        by.x = c("referenceScenario", "pkParameter", "outputPathId"),
        by.y = c("scenario", "pkParameter", "outputPathId"),
        suffixes = c("", ".reference")
      )

    plotData[, xValues := xValues / xValues.reference]

    plotData[!is.na(xMin), xMin := xMin / ifelse(is.na(xMax.reference), xValues.reference, xMax.reference)]
    plotData[!is.na(xMax), xMax := xMax / ifelse(is.na(xMin.reference), xValues.reference, xMin.reference)]
  } else {
    plotData <- calculateAggregationWithCIBYGroup(
      dt = pkParameterFiltered,
      aggregationFun = aggregationFun,
      confLevel = confLevel,
      nBootstrap = nBootstrap,
      valueColumn = "value",
      identifier = c("pkParameter", "outputPathId", "scenario"),
      direction = "x"
    ) %>%
      checkPrecision()
  }

  return(plotData)
}


#' Check Precision of xValues
#'
#' This function calculates the precision of xValues in a data.table based on the provided
#' xMin and xMax columns. If the precision is below a specified threshold, it sets xMin and
#' xMax to NA and calculates an estimated N value. It also logs a message if the required
#' precision is not reached.
#'
#' @param dt A data.table containing the columns xValues, xMin, and xMax.
#'
#' @return A data.table with updated xMin and xMax values, and an additional column
#'         estimatedN if the required precision is not reached.
#'
#' @details The precision is calculated as xValues / (xMax - xMin). The precision threshold
#'          is retrieved from the options with a default value of 0.01. If any xMin values
#'          remain, a message is logged indicating that the required precision was not reached.
#'
#' @keywords internal
checkPrecision <- function(dt) {
  # initialize to avoid linter messages
  xMin <- xMax <- xValues <- precision <- NULL

  dt[, precision := (xMax - xMin) / xValues]

  precisionThreshold <- getOption("OSPSuite.RF.RequiredPrecisison", default = 0.01)

  dt[precision <= precisionThreshold, `:=`(xMin = NA, xMax = NA)]

  if (any(!is.na(dt$xMin))) {
    writeToLog(
      type = "Info",
      msg = "Required presicison was not reached, please rerurn simulations with larger N"
    )
    writeTableToLog(dt[!is.na(xMin)])
  }

  return(dt)
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
  # initialize to avoid linter messages
  dataType <- NULL

  plotData[, dataType := "simulated"]
  # add datagroupId
  configToFilter <- onePlotConfig[, c(
    "pkParameter", "outputPathId", "scenario", "referenceScenario",
    "dataGroupId"
  )] %>%
    unique()

  plotData <- merge(
    configToFilter,
    plotData,
    by = intersect(c(
      "pkParameter",
      "outputPathId",
      "scenario",
      "referenceScenario"
    ), names(plotData))
  )

  # add observedData
  if (!is.null(pkParameterObserved) && nrow(pkParameterObserved) > 0) {
    tmp <- pkParameterObserved %>%
      merge(configToFilter,
        by = c("dataGroupId", "pkParameter", "outputPathId")
      ) %>%
      dplyr::select(dplyr::any_of(names(plotData)))
    plotData <- rbind(plotData,
      tmp,
      fill = TRUE
    )
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
addDescriptions <- function(plotData, onePlotConfig, pkParameterDT) {
  # initialize to avoid linter messages
  pkParameter <- outputPathId <- NULL

  plotData <- merge(plotData,
    onePlotConfig[, c("scenario", "scenarioShortName", "scenarioGroup")] %>% unique(),
    by = "scenario"
  )

  plotData$scenarioShortName <- factor(
    plotData$scenarioShortName,
    levels = unique(onePlotConfig$scenarioShortName),
    ordered = TRUE
  )

  #' Add Output Path Details to Plot Data
  plotData <- merge(plotData,
    configEnv$outputPaths[, c("outputPathId", "displayNameOutput", "displayUnit")] %>%
      unique(),
    by = "outputPathId"
  )

  #' Add PK Parameter Details to Plot Data
  plotData <- plotData %>%
    merge(
      pkParameterDT[, c("pkParameter", "displayNamePKParameter", "displayUnitPKParameter")] %>%
        unique(),
      by = c("pkParameter")
    )

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
#' @return A named vector of table labels.
#' @keywords internal
getTableLabelsForPKForest <- function(plotData) {
  tableColumns <- intersect(c("xValues", "xErrorValues", "xMin", "xMax"), names(plotData))
  tableLabels <- getErrorLabels(plotData$xErrorType[1])[seq(1, length(tableColumns))]

  names(tableLabels) <- tableColumns

  return(tableLabels)
}
#' Get Mapping for Forest Plots
#'
#' This function generates a mapping for forest plots based on the provided plot data
#' and the specified column list. It uses the `aes` function from ggplot2 to create
#' the mapping, which includes x and y values, and optionally includes error bars
#' based on the plot data provided.
#'
#' @param plotData A data frame containing the data to be plotted. It should include
#'   columns for x values, y values, and optionally error values.
#' @param columnList A list containing the names of the columns to be used in the
#'   mapping. It should include at least `yColumn` for the y-axis.
#'
#' @return A mapping object created using `aes` from ggplot2, which can be used
#'   in a ggplot call to create forest plots.
#'   @keywords internal
getMappingForForestPlots <- function(plotData,columnList){
  mapping <- aes(x = xValues,y = get(columnList$yColumn),groupby = dataType)

  mapping <- utils::modifyList(
    mapping,
    eval(parse(text = paste0("aes(y = ",columnList$yColumn,")")))
    )
  if ('xMin' %in% names(plotData) & 'xMax' %in% names(plotData)){
    mapping <- utils::modifyList(mapping,
                                 aes(xmin = xMin, xmax = xMax))
  } else if( 'xErrorValues' %in% names(plotData)){
    if (plotData$xErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev){
      mapping <- utils::modifyList(mapping,
                                   aes(error = xErrorValues))
    } else if (plotData$xErrorType[1] == ospsuite::DataErrorType$GeometricStdDev){
      mapping <- utils::modifyList(mapping,
                                   aes(error_relative = xErrorValues))
    }
  }

  return(mapping)
}


#' Get Column Selection for PK Forest
#'
#' Selects columns for the PK forest plot based on the provided data and ratio mode.
#'
#' @param plotData Data table containing plot data.
#' @param ratioMode Mode for ratio calculations.
#' @return A list of selected columns for the plot.
#' @keywords internal
getColumnSelectionForPKForest <- function(plotData, ratioMode) {
  columnList <- list()
  if (ratioMode == "none") {
    columnList$yColumn <- "scenarioShortName"
    columnList$yFacetColumns <- c("scenarioGroup")
    columnList$xLabel <- plotData$displayNamePKParameter[1]
    pkUnit <- plotData$displayUnitPKParameter[1]
    if (pkUnit != "") columnList$xLabel <- paste0(columnList$xLabel, " [", pkUnit, "]")
  } else {
    columnList$yColumn <- "displayNamePKParameter"
    columnList$yFacetColumns <- c("scenarioGroup", "scenarioShortName")
    columnList$xLabel <- "Ratio"
  }

  if (uniqueN(plotData[[columnList$yColumn]]) == 1) {
    columnList$xLabel <- paste(plotData[[columnList$yColumn]][1], columnList$xLabel)
    columnList$yColumn <- tail(columnList$yFacetColumns, 1)
    columnList$yFacetColumns <- setdiff(columnList$yFacetColumns, columnList$yColumn)
  }


  if (uniqueN(plotData$plotTag) > 1) {
    columnList$xFacetColumn <- "plotTag"
  } else {
    columnList$xFacetColumn <- NULL
  }
  return(columnList)
}
#' Adjust PK Forest Plot Object
#'
#' Adjusts aesthetics of the PK forest plot object.
#'
#' @param combinedObject The plot object to adjust.
#' @param scaleVectors A list defining colors, fills, and shapes for the plot.
#' @param vlineIntercept Optional vertical line intercept.
#' @param tableLabels named vector with table headers
#' @return The adjusted plot object.
#' @keywords internal
adjustPkForestPlotObject <- function(combinedObject, scaleVectors, vlineIntercept, tableLabels) {
  combinedObject$plotObject <- combinedObject$plotObject +
    theme(
      legend.title = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5)
    ) +
    scale_color_manual(values = unlist(lapply(scaleVectors, getElement, "color"))) +
    scale_fill_manual(values = unlist(lapply(scaleVectors, getElement, "fill"))) +
    scale_shape_manual(values = unlist(lapply(scaleVectors, getElement, "shape")))

  if (!is.null(vlineIntercept)) {
    combinedObject$plotObject <- combinedObject$plotObject +
      geom_vline(xintercept = vlineIntercept)
  }
  if (dplyr::n_distinct(combinedObject$plotObject$data$dataType) == 1) {
    combinedObject$plotObject <- combinedObject$plotObject +
      theme(legend.position = "none")
  } else {
    combinedObject$plotObject <- combinedObject$plotObject +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
  }

  {
    combinedObject$plotObject <- combinedObject$plotObject +
      theme(axis.text.x.top = element_text(angle = 0))
  }

  combinedObject$tableObject <-
    combinedObject$tableObject +
    if (length(tableLabels) == 1){
      theme(axis.text.x = element_text(
        angle = 0,
        vjust = 0.5,
        hjust = 0.5
    ))
    } else {
      theme(axis.text.x = element_text(
        angle = -30,
        vjust = 1,
        hjust = 1
      ))
    }
  return(combinedObject)
}

addPrecisisonWatermark <- function(combinedObject, asPointeEstimate, plotData) {
  # initialize to avoid linter messages
  dataType <- NULL

  if (!asPointeEstimate) {
    return(combinedObject)
  }
  # precisison was reached for all entries
  if (all(is.na(plotData[dataType == "simulated"]$xMin))) {
    return(combinedObject)
  }

  combinedObject$plotObject <- combinedObject$plotObject +
    layerWatermark(
      label = "Outside precisison requirement",
      color = "red",
      show = TRUE,
      x = 0.6
    )

  return(combinedObject)
}

#' Get Caption for Forest Plot
#'
#' Generates a caption for the forest plot based on the provided data.
#'
#' @param plotData Data table containing plot data.
#' @param xScale Scale of the x-axis.
#' @param plotCaptionAddon Additional caption text.
#' @param ratioMode Mode for ratio calculations.
#' @param asPointeEstimate Logical indicating if confidence intervals should be calculated.
#' @param nameOfPointestimate description of aggregation quantity
#' @return A string containing the caption for the plot.
#' @keywords internal
getCaptionForForestPlot <- function(plotData,
                                    xScale,
                                    plotCaptionAddon,
                                    ratioMode) {
  # initialize to avoid linter messages
  scenarioShortName <- N <- NULL # nolint

  dtCaption <-
    plotData[, c(
      "displayNameOutput",
      "plotTag"
    )] %>% unique()

  if (ratioMode == "none") {
    pktext <- concatWithAnd(unique(plotData$displayNamePKParameter))
  } else {
    pktext <- concatWithAnd(paste(unique(plotData$displayNamePKParameter), "ratios"))
  }

  captiontxt <- paste0(
    "Simulated ",
    ifelse("observed" %in% unique(plotData$type), " and observed ", ""),
    pktext,
    " of ",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"),
    " on a ", ifelse(xScale == "linear", "linear", "logarithmic"),
    " x-scale."
  )

  # add number of individuals
  if (dplyr::n_distinct(plotData$N) == 1) {
    nTxt <- paste("Number of simulated values is", plotData$N[1], "for all scenarios")
  } else {
    tmp <- plotData[, .(N = pasteFigureTags(.SD, "N", endWithDot = FALSE)), by = "scenarioShortName"]
    tmp <- tmp[, .(scenariosPerN = paste(N, "for", concatWithAnd(unique(scenarioShortName)))), by = c("N")]
    nTxt <- paste("Number of simulated values is", concatWithAnd(tmp$scenariosPerN))
  }
  captiontxt <- addCaptionTextAddon(captiontxt, nTxt)

  captiontxt <- addCaptionTextAddon(captiontxt, plotCaptionAddon)

  return(captiontxt)
}
#' Get Footnote Lines for Forest Plots
#'
#' Generates footnote lines for the forest plot.
#'
#' @param plotData Data table containing plot data.
#' @param ratioMode Mode for ratio calculations.
#' @param asPointeEstimate Logical indicating if confidence intervals should be calculated.
#' @param dtDataReference Optional data reference.
#' @return A character vector containing footnote lines.
#' @keywords internal
getFootnoteLinesForForrestPlots <- function(plotData, ratioMode, asPointeEstimate, dtDataReference) {
  errorLabels <- getErrorLabels(plotData$xErrorType[1])

  footnoteLines <-
    paste0(
      "Simulated ",
      ifelse("observed" %in% unique(plotData$type), " and observed ", ""),
      "data is displayed as ",
      concatWithAnd(errorLabels),
      "."
    )
  if (ratioMode == "ratioOfPopulation") {
    footnoteLines <- c(
      footnoteLines,
      "Ratios were calculated as ratios of population summary statistics."
    )
  }

  return(footnoteLines)
}

#' Get Ratio Mode for Plot Configuration
#'
#' This function determines the ratio mode for a given plot configuration based on the provided parameters.
#' It checks if the scenarios in the plot configuration have the same or different base populations.
#'
#' @param onePlotConfig A data frame containing the plot configuration with columns 'plotName', 'scenario', and 'referenceScenario'.
#' @param pkParameterDT A data frame containing parameter details, including 'scenario' and 'populationId'.
#' @param asRatio A logical value indicating whether to calculate the ratio mode. If FALSE, the function returns 'none'.
#'
#' @return A character string indicating the ratio mode. Possible values are:
#' - 'none' if `asRatio` is FALSE.
#' - 'individualRatios' if all population IDs match between scenarios.
#' - 'ratioOfPopulation' if all population IDs are different between scenarios.
#'
#' @keywords internal
getRatioMode <- function(onePlotConfig, pkParameterDT, asRatio) {
  if (!asRatio) {
    return("none")
  }

  pkParameterDTScenarios <- pkParameterDT[, c("scenario", "populationId")] %>%
    unique()

  dtPop <- merge(
    onePlotConfig[, c("plotName", "scenario", "referenceScenario")] %>%
      unique(),
    pkParameterDTScenarios,
    by = "scenario"
  ) %>%
    merge(pkParameterDTScenarios,
      by.x = "referenceScenario",
      by.y = "scenario",
      suffixes = c("", ".reference")
    )

  if (all(dtPop$populationId == dtPop$populationId.reference)) {
    ratioMode <- "individualRatios"
  } else if (all(dtPop$populationId != dtPop$populationId.reference)) {
    ratioMode <- "ratioOfPopulation"
  } else {
    print(dtPop)
    stop("Within one plot you must either compare always scenarios with the same base population or
             always scenarios with different base populations")
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
validatePKForestAggregatedAbsoluteValuesConfig <-
  function(configTable, pkParameterDT, ...) {
    validatePKForestConfigTable(
      configTable = configTable,
      pkParameterDT = pkParameterDT,
      ...
    )
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
validatePKForestPointEstimateOfAbsoluteValuesConfig <-
  function(configTable, pkParameterDT, ...) {
    validatePKForestConfigTable(
      configTable = configTable,
      pkParameterDT = pkParameterDT,
      ...
    )
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
validatePKForestAggregatedRatiosConfig <-
  function(configTable, pkParameterDT, ...) {
    configTablePlots <- validatePKForestConfigTable(
      configTable = configTable,
      pkParameterDT = pkParameterDT,
      ...
    )

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
validatePKForestPointEstimateOfRatiosConfig <-
  function(configTable, pkParameterDT, ...) {
    configTablePlots <- validatePKForestConfigTable(
      configTable = configTable,
      pkParameterDT = pkParameterDT,
      ...
    )

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
validatePKForestConfigTable <- function(configTable, pkParameterDT, ...) {
  configTablePlots <- validateHeaders(configTable)
  dotArgs <- list(...)
  pkParameterObserved <- dotArgs$pkParameterObserved

  if (!is.null(pkParameterObserved)) {
    checkmate::assertDataTable(pkParameterObserved)
    if (!DATACLASS$pkAggregated %in% unique(pkParameterObserved$dataClass)) {
      stop("Please provide aggregated observed PK-Parameter data for this kind of plot ")
    }
  }

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("plotName", "scenarioShortName", "scenario"),
    charactersWithMissing = c("scenarioGroup", "plotCaptionAddon", "dataGroupId"),
    logicalColumns = NULL,
    numericRangeColumns = c("xlimit_linear", "xlimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("scenario", "referenceScenario"),
        allowedValues = unique(pkParameterDT$scenario)
      ),
      pkParameter = list(
        cols = c("pkParameters"),
        allowedValues = unique(pkParameterDT$pkParameter)
      ),
      outputPathId = list(
        cols = c("outputPathIds"),
        allowedValues = unique(pkParameterDT$outputPathId)
      ),
      dataGroupId = list(
        cols = c("dataGroupId"),
        allowedValues = unique(pkParameterObserved$group),
        splitAllowed = FALSE
      ),
      facetScale = list(
        cols = c("facetScale"),
        allowedValues = c('free','free_y'),
        splitAllowed = FALSE
      )

    )
  )

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "plotCaptionAddon",
      "outputPathIds"
    )
  )

  return(configTablePlots)
}
#' Validate Bootstrapping Inputs
#'
#' Validates the inputs for bootstrapping in PK forest plots.
#'
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @param statFun Statistical function for bootstrapping.
#' @param ratioMode Mode for ratio calculations.
#' @return NULL (invisible).
#' @keywords internal
validatePointEstimateInputs <- function(nBootstrap, confLevel, statFun) {
  checkmate::assertIntegerish(nBootstrap, lower = 0, len = 1)
  checkmate::assertDouble(confLevel, lower = 0, upper = 1, len = 1, finite = TRUE)
  checkmate::assertFunction(statFun[[1]])
  checkmate::assertNamed(statFun)

  return(invisible())
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
                                 vlineIntercept) {
  validatePKParameterDT(pkParameterDT)
  # Check if scaleVectors is a list
  checkmate::assertList(scaleVectors)
  if (length(scaleVectors) > 0) {
    # Check if it contains the expected names
    checkmate::assertNames(names(scaleVectors), subset.of = c("simulated", "observed"))

    # Check each entry to ensure they are lists with the correct names
    for (entry in scaleVectors) {
      checkmate::assertList(entry, names = "named")
      checkmate::assertNames(names(entry), subset.of = c("color", "fill", "shape"))
    }
  }

  checkmate::assertNumeric(labelWrapWidth, lower = 0, len = 1)
  checkmate::assertNumeric(vlineIntercept, null.ok = TRUE)

  return(invisible())
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
#' \code{\link{plotPKForestAggregatedAbsoluteValues}}, \code{\link{plotPKForestPointEstimateOfAbsoluteValues}},
#' \code{\link{plotPKForestAggregatedRatios}}, \code{\link{plotPKForestPointEstimateOfRatios}},
#
#' @export
addDefaultConfigForPKForestPlots <- function(projectConfiguration,
                                             pkParameterDT,
                                             sheetName = "PKParameter_Forest",
                                             overwrite = FALSE) {
  # avoid warnings for global variables during check
  scenarioName <- outputPathId <- outputPathIds <- pkParameter <- pkParameters <- NULL

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
  dt <- pkParameterDT[, .(
    pkParameters = paste(unique(pkParameter), collapse = ", "),
    outputPathIds = paste(unique(outputPathId), collapse = ", ")
  )]


  # Create a new data.table with all combinations of pkParameters and scenario names
  dtNewConfig <- dt[, .(
    scenario = scenarios$scenarioName,
    plotName = "PKForest",
    xScale = "linear,log",
    facetScale = "free_y"
  ),
  by = .(outputPathIds, pkParameters)
  ]


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

  return(invisible())
}
